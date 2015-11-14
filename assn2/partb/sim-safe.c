/* sim-safe.c - sample functional simulator implementation */

/* SimpleScalar(TM) Tool Suite
 * Copyright (C) 1994-2003 by Todd M. Austin, Ph.D. and SimpleScalar, LLC.
 * All Rights Reserved. 
 * 
 * THIS IS A LEGAL DOCUMENT, BY USING SIMPLESCALAR,
 * YOU ARE AGREEING TO THESE TERMS AND CONDITIONS.
 * 
 * No portion of this work may be used by any commercial entity, or for any
 * commercial purpose, without the prior, written permission of SimpleScalar,
 * LLC (info@simplescalar.com). Nonprofit and noncommercial use is permitted
 * as described below.
 * 
 * 1. SimpleScalar is provided AS IS, with no warranty of any kind, express
 * or implied. The user of the program accepts full responsibility for the
 * application of the program and the use of any results.
 * 
 * 2. Nonprofit and noncommercial use is encouraged. SimpleScalar may be
 * downloaded, compiled, executed, copied, and modified solely for nonprofit,
 * educational, noncommercial research, and noncommercial scholarship
 * purposes provided that this notice in its entirety accompanies all copies.
 * Copies of the modified software can be delivered to persons who use it
 * solely for nonprofit, educational, noncommercial research, and
 * noncommercial scholarship purposes provided that this notice in its
 * entirety accompanies all copies.
 * 
 * 3. ALL COMMERCIAL USE, AND ALL USE BY FOR PROFIT ENTITIES, IS EXPRESSLY
 * PROHIBITED WITHOUT A LICENSE FROM SIMPLESCALAR, LLC (info@simplescalar.com).
 * 
 * 4. No nonprofit user may place any restrictions on the use of this software,
 * including as modified by the user, by any other authorized user.
 * 
 * 5. Noncommercial and nonprofit users may distribute copies of SimpleScalar
 * in compiled or executable form as set forth in Section 2, provided that
 * either: (A) it is accompanied by the corresponding machine-readable source
 * code, or (B) it is accompanied by a written offer, with no time limit, to
 * give anyone a machine-readable copy of the corresponding source code in
 * return for reimbursement of the cost of distribution. This written offer
 * must permit verbatim duplication by anyone, or (C) it is distributed by
 * someone who received only the executable form, and is accompanied by a
 * copy of the written offer of source code.
 * 
 * 6. SimpleScalar was developed by Todd M. Austin, Ph.D. The tool suite is
 * currently maintained by SimpleScalar LLC (info@simplescalar.com). US Mail:
 * 2395 Timbercrest Court, Ann Arbor, MI 48105.
 * 
 * Copyright (C) 1994-2003 by Todd M. Austin, Ph.D. and SimpleScalar, LLC.
 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "host.h"
#include "misc.h"
#include "machine.h"
#include "regs.h"
#include "memory.h"
#include "loader.h"
#include "syscall.h"
#include "options.h"
#include "stats.h"
#include "sim.h"

#define NUM_OFFBITS 32
#define NUM_REGS 32

/*
 * This file implements a functional simulator.  This functional simulator is
 * the simplest, most user-friendly simulator in the simplescalar tool set.
 * Unlike sim-fast, this functional simulator checks for all instruction
 * errors, and the implementation is crafted for clarity rather than speed.
 */

 /* to determine total number of specific types of instructions */
static counter_t g_total_cond_branches;
static counter_t g_total_uncond_branches;
static counter_t g_total_fp_inst;
static counter_t g_total_store_inst;
static counter_t g_total_ld_inst;
static counter_t g_total_imm_inst;

// ASSIGNMENT 2
static float cond_freq;
static float uncond_freq;
//static float fp_freq;
static float store_freq;
static float ld_freq;
//static float imm_freq;


static counter_t histogram[NUM_OFFBITS]; // histogram array for question 2
static counter_t reg_contents[NUM_REGS]; // total number of bits changed for particular GPR
static signed int prev_val[NUM_REGS]; // array saving the previous values of O1
static counter_t num_inst_chng_bits; // total number of write-to-GPR instructions (used for question 3)

 
/* simulated registers */
static struct regs_t regs;

/* simulated memory */
static struct mem_t *mem = NULL;

/* track number of refs */
static counter_t sim_num_refs = 0;

/* maximum number of inst's to execute */
static unsigned int max_insts;

/* register simulator-specific options */
void
sim_reg_options(struct opt_odb_t *odb)
{
  opt_reg_header(odb, 
"sim-safe: This simulator implements a functional simulator.  This\n"
"functional simulator is the simplest, most user-friendly simulator in the\n"
"simplescalar tool set.  Unlike sim-fast, this functional simulator checks\n"
"for all instruction errors, and the implementation is crafted for clarity\n"
"rather than speed.\n"
		 );

  /* instruction limit */
  opt_reg_uint(odb, "-max:inst", "maximum number of inst's to execute",
	       &max_insts, /* default */0,
	       /* print */TRUE, /* format */NULL);

}

/* check simulator-specific option values */
void
sim_check_options(struct opt_odb_t *odb, int argc, char **argv)
{
  /* nada */
}

/* register simulator-specific statistics */
void
sim_reg_stats(struct stat_sdb_t *sdb)
{
  stat_reg_counter(sdb, "sim_num_insn",
		   "total number of instructions executed",
		   &sim_num_insn, sim_num_insn, NULL);
 

  stat_reg_counter(sdb, "sim_num_cond_branches" /* label for printing */,
			"total conditional branches executed" /* description */,
			&g_total_cond_branches /* pointer to the counter */,
			0 /* initial value for the counter */, NULL);
  stat_reg_formula(sdb, "sim_cond_branch_freq",
			"relative frequency of conditional branches",
			"sim_num_cond_branches / sim_num_insn", NULL);
  stat_reg_counter(sdb, "sim_num_uncond_branches",
			"total unconditional branches executed",
			&g_total_uncond_branches, 0, NULL);
  stat_reg_formula(sdb, "sim_uncond_branch_freq",
			"relative frequency of unconditional branches",
			"sim_num_uncond_branches / sim_num_insn", NULL);
  stat_reg_counter(sdb, "sim_num_fp_inst",
			"total number of floating-point instructions",
			&g_total_fp_inst, 0, NULL);
  stat_reg_formula(sdb, "sim_fp_inst_freq",
			"relative frequency of fp instructions",
			"sim_num_fp_inst / sim_num_insn", NULL);
  stat_reg_counter(sdb, "sim_num_store_inst",
			"total number of store instructions",
			&g_total_store_inst, 0, NULL);
  stat_reg_formula(sdb, "sim_store_inst_freq",
			"relative frequency of store instructions",
			"sim_num_store_inst / sim_num_insn", NULL);
  stat_reg_counter(sdb, "sim_num_ld_inst",
			"total number of load instructions",
			&g_total_ld_inst, 0, NULL);
  stat_reg_formula(sdb, "sim_ld_inst_freq",
			"relative frequency of load instructions",
			"sim_num_ld_inst / sim_num_insn", NULL);
  stat_reg_counter(sdb, "sim_num_imm_inst",
			"total number of instructions with immediate operands",
			&g_total_imm_inst, 0, NULL);
  stat_reg_formula(sdb, "sim_imm_inst_freq",
			"relative frequency of immediate instructions",
			"sim_num_imm_inst / sim_num_insn", NULL);

  
  stat_reg_counter(sdb, "sim_num_refs",
		   "total number of loads and stores executed",
		   &sim_num_refs, 0, NULL);
  stat_reg_int(sdb, "sim_elapsed_time",
	       "total simulation time in seconds",
	       &sim_elapsed_time, 0, NULL);
  stat_reg_formula(sdb, "sim_inst_rate",
		   "simulation speed (in insts/sec)",
		   "sim_num_insn / sim_elapsed_time", NULL);
  ld_reg_stats(sdb);
  mem_reg_stats(mem, sdb);
}

/* initialize the simulator */
void
sim_init(void)
{
  sim_num_refs = 0;

  /* allocate and initialize register file */
  regs_init(&regs);

  /* allocate and initialize memory space */
  mem = mem_create("mem");
  mem_init(mem);
}

/* load program into simulated state */
void
sim_load_prog(char *fname,		/* program to load */
	      int argc, char **argv,	/* program arguments */
	      char **envp)		/* program environment */
{
  /* load program text and data, set up environment, memory, and regs */
  ld_load_prog(fname, argc, argv, envp, &regs, mem, TRUE);
}

/* print simulator-specific configuration information */
void
sim_aux_config(FILE *stream)		/* output stream */
{
  /* nothing currently */
}

/* dump simulator-specific auxiliary simulator statistics */
void
sim_aux_stats(FILE *stream)		/* output stream */
{
  int i = 0;
  long total_offbits = 0;
  double avg_bits_changed = 0.0;
  
  while(i<NUM_OFFBITS){
	fprintf(stream, "with offset %d: %d\n", i, histogram[i]);
	total_offbits += histogram[i];
	i++;
  }
  
  fprintf(stream, "total cond branches used: %li\n", total_offbits);
  
  for(i=0; i<NUM_REGS; i++){
	avg_bits_changed += reg_contents[i];
  }
  fprintf(stream, "avg number of bits changed: %lf\n", avg_bits_changed/num_inst_chng_bits);
  
  // ASSIGNMENT 2
  cond_freq = (float)(g_total_cond_branches)/(float)sim_num_insn;
  uncond_freq = (float)(g_total_uncond_branches)/(float)sim_num_insn;
  store_freq = (float)(g_total_store_inst)/(float)sim_num_insn;
  ld_freq = (float)(g_total_ld_inst)/(float)sim_num_insn;


 fprintf(stream, "CPI=%f\n",
	 1+(uncond_freq)+(cond_freq*(1-cond_freq - uncond_freq - store_freq - ld_freq))+2*(cond_freq*ld_freq)+(ld_freq*(1-cond_freq - uncond_freq - store_freq - ld_freq)));
}

/* un-initialize simulator-specific state */
void
sim_uninit(void)
{
  /* nada */
}


// initialize arrays to be used
void init_arrays(void){
	
	int offbits = 0;
	int regnum = 0;
	
	// initialize histogram
	while(offbits<NUM_OFFBITS){
		histogram[offbits] = 0;
		offbits++;
	}
	
	// initialize reg_contents array
	while(regnum < NUM_REGS){
		reg_contents[regnum] = 0;
		regnum++;
	}
	
	// initialize previous-value of register array
	for(regnum=0; regnum < NUM_REGS; regnum++){
		prev_val[regnum] = 0;
	}
	
	// initialize total number of write-to-GPR instructions to 0
	num_inst_chng_bits = 0;
}

// find how many bits were changed
int func_bits_chng(signed int before, signed int after){
	
	int bits_changed = 0;
	signed int a = before;
	signed int b = after;
	int counter_temp = 32; // assuming GPRs contain 32-bit values
	
	while(counter_temp > 0){
		if((a & 0x01) != (b & 0x01)){
			bits_changed++;
		}
		a = a >> 1;
		b = b >> 1;
		counter_temp--;
	}
	
	/*
	signed int ans = before ^ after;
	
	while(ans != 0){
		if((ans & 0x1) == 1) bits_changed++;
		ans = ans >> 1;
	}
	*/
	return bits_changed;
}


/*
 * configure the execution engine
 */

/*
 * precise architected register accessors
 */

/* next program counter */
#define SET_NPC(EXPR)		(regs.regs_NPC = (EXPR))

/* current program counter */
#define CPC			(regs.regs_PC)

/* general purpose registers */
#define GPR(N)			(regs.regs_R[N])
#define SET_GPR(N,EXPR)		(regs.regs_R[N] = (EXPR))

#if defined(TARGET_PISA)

/* floating point registers, L->word, F->single-prec, D->double-prec */
#define FPR_L(N)		(regs.regs_F.l[(N)])
#define SET_FPR_L(N,EXPR)	(regs.regs_F.l[(N)] = (EXPR))
#define FPR_F(N)		(regs.regs_F.f[(N)])
#define SET_FPR_F(N,EXPR)	(regs.regs_F.f[(N)] = (EXPR))
#define FPR_D(N)		(regs.regs_F.d[(N) >> 1])
#define SET_FPR_D(N,EXPR)	(regs.regs_F.d[(N) >> 1] = (EXPR))

/* miscellaneous register accessors */
#define SET_HI(EXPR)		(regs.regs_C.hi = (EXPR))
#define HI			(regs.regs_C.hi)
#define SET_LO(EXPR)		(regs.regs_C.lo = (EXPR))
#define LO			(regs.regs_C.lo)
#define FCC			(regs.regs_C.fcc)
#define SET_FCC(EXPR)		(regs.regs_C.fcc = (EXPR))

#elif defined(TARGET_ALPHA)

/* floating point registers, L->word, F->single-prec, D->double-prec */
#define FPR_Q(N)		(regs.regs_F.q[N])
#define SET_FPR_Q(N,EXPR)	(regs.regs_F.q[N] = (EXPR))
#define FPR(N)			(regs.regs_F.d[(N)])
#define SET_FPR(N,EXPR)		(regs.regs_F.d[(N)] = (EXPR))

/* miscellaneous register accessors */
#define FPCR			(regs.regs_C.fpcr)
#define SET_FPCR(EXPR)		(regs.regs_C.fpcr = (EXPR))
#define UNIQ			(regs.regs_C.uniq)
#define SET_UNIQ(EXPR)		(regs.regs_C.uniq = (EXPR))

#else
#error No ISA target defined...
#endif

/* precise architected memory state accessor macros */
#define READ_BYTE(SRC, FAULT)						\
  ((FAULT) = md_fault_none, addr = (SRC), MEM_READ_BYTE(mem, addr))
#define READ_HALF(SRC, FAULT)						\
  ((FAULT) = md_fault_none, addr = (SRC), MEM_READ_HALF(mem, addr))
#define READ_WORD(SRC, FAULT)						\
  ((FAULT) = md_fault_none, addr = (SRC), MEM_READ_WORD(mem, addr))
#ifdef HOST_HAS_QWORD
#define READ_QWORD(SRC, FAULT)						\
  ((FAULT) = md_fault_none, addr = (SRC), MEM_READ_QWORD(mem, addr))
#endif /* HOST_HAS_QWORD */

#define WRITE_BYTE(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, addr = (DST), MEM_WRITE_BYTE(mem, addr, (SRC)))
#define WRITE_HALF(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, addr = (DST), MEM_WRITE_HALF(mem, addr, (SRC)))
#define WRITE_WORD(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, addr = (DST), MEM_WRITE_WORD(mem, addr, (SRC)))
#ifdef HOST_HAS_QWORD
#define WRITE_QWORD(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, addr = (DST), MEM_WRITE_QWORD(mem, addr, (SRC)))
#endif /* HOST_HAS_QWORD */

/* system call handler macro */
#define SYSCALL(INST)	sys_syscall(&regs, mem_access, mem, INST, TRUE)

#define DNA         (-1)

/* general register dependence decoders */
#define DGPR(N)         (N)
#define DGPR_D(N)       ((N) &~1)

/* floating point register dependence decoders */
#define DFPR_L(N)       (((N)+32)&~1)
#define DFPR_F(N)       (((N)+32)&~1)
#define DFPR_D(N)       (((N)+32)&~1)

/* miscellaneous register dependence decoders */
#define DHI         (0+32+32)
#define DLO         (1+32+32)
#define DFCC            (2+32+32)
#define DTMP            (3+32+32)

/* start simulation, program loaded, processor precise state initialized */
void
sim_main(void)
{
  md_inst_t inst;
  register md_addr_t addr;
  enum md_opcode op;
  register int is_write;
  enum md_fault_type fault;

  // conditional-branch offset bits
  int offbits = 0;
  int twoscompl = 0;
  unsigned int abs_offbits = 0;
  
  // number of bits changed in a GPR
  int bits_changed;
  
  int dst_reg; // destination register in part 3

  fprintf(stderr, "sim: ** starting functional simulation **\n");

  /* set up initial default next PC */
  regs.regs_NPC = regs.regs_PC + sizeof(md_inst_t);

	// initialize all the arrays
	init_arrays();
	
		
  while (TRUE)
    {
      /* maintain $r0 semantics */
      regs.regs_R[MD_REG_ZERO] = 0;
#ifdef TARGET_ALPHA
      regs.regs_F.d[MD_REG_ZERO] = 0.0;
#endif /* TARGET_ALPHA */

      /* get the next instruction to execute */
      MD_FETCH_INST(inst, mem, regs.regs_PC);

      /* keep an instruction count */
      sim_num_insn++;

      /* set default reference address and access mode */
      addr = 0; is_write = FALSE;

      /* set default fault - none */
      fault = md_fault_none;

      /* decode the instruction */
      MD_SET_OPCODE(op, inst);

      /* execute the instruction */
      switch (op)
	{
#define DEFINST(OP,MSK,NAME,OPFORM,RES,FLAGS,O1,O2,I1,I2,I3)		\
	case OP:							\
		  dst_reg = O1;					\
		  if(dst_reg > 0 && dst_reg < NUM_REGS) prev_val[dst_reg] = regs.regs_R[dst_reg]; \
          SYMCAT(OP,_IMPL);						\
          break;
#define DEFLINK(OP,MSK,NAME,MASK,SHIFT)					\
        case OP:							\
          panic("attempted to execute a linking opcode");
#define CONNECT(OP)
#define DECLARE_FAULT(FAULT)						\
	  { fault = (FAULT); break; }
#include "machine.def"
	default:
	  panic("attempted to execute a bogus opcode");
      }

      if (fault != md_fault_none)
	fatal("fault (%d) detected @ 0x%08p", fault, regs.regs_PC);

      if (verbose)
	{
	  myfprintf(stderr, "%10n [xor: 0x%08x] @ 0x%08p: ",
		    sim_num_insn, md_xor_regs(&regs), regs.regs_PC);
	  md_print_insn(inst, regs.regs_PC, stderr);
	  if (MD_OP_FLAGS(op) & F_MEM)
	    myfprintf(stderr, "  mem: 0x%08p", addr);
	  fprintf(stderr, "\n");
	  /* fflush(stderr); */
	}
	
	
	// for registers 0-31 determine the new value written to dst_reg
	if(dst_reg > 0 && dst_reg < NUM_REGS){
		
		bits_changed = func_bits_chng(prev_val[dst_reg], regs.regs_R[dst_reg]);
		reg_contents[dst_reg] += bits_changed;
		num_inst_chng_bits++;
		
	}
	
	
	  if(MD_OP_FLAGS(op) & F_COND)
	{
		g_total_cond_branches++;
		
		// increment histogram index with corresp. offset bits
		offbits = (regs.regs_TPC - regs.regs_PC)/8;
		
		// absolute value representation of the offbits
		abs_offbits = abs(regs.regs_TPC - regs.regs_PC)/8;
				
		if(offbits < 0){
			// if offset is negative, simply add one bit (the signed bit) to its absolute value
			// this is equivalent to offbits = ceil(log10(-1*offbits)/log10(2) + 1); (trust me, I've tried it)
			offbits = floor(log10(abs_offbits)/log10(2) + 2) + 1; 
		}else{
			offbits = floor(log10(abs_offbits)/log10(2) + 2);
		}
		
		
		histogram[offbits]++;

	}
	
	  if(MD_OP_FLAGS(op) & F_DIRJMP)
	{
		g_total_uncond_branches++;
		
	}
	
	  if((MD_OP_FLAGS(op) & F_FCOMP) || (MD_OP_FLAGS(op) & F_FPCOND))
	{
		g_total_fp_inst++;
	}
	
	  if(MD_OP_FLAGS(op) & F_STORE)
	{
		g_total_store_inst++;
	}
	
	  if(MD_OP_FLAGS(op) & F_LOAD)
	{
		g_total_ld_inst++;
	}
	
	  if(MD_OP_FLAGS(op) & F_IMM)
	{
		g_total_imm_inst++;
	}
	
      if (MD_OP_FLAGS(op) & F_MEM)
	{
	  sim_num_refs++;
	  if (MD_OP_FLAGS(op) & F_STORE)
	    is_write = TRUE;
	}

	

      /* go to the next instruction */
      regs.regs_PC = regs.regs_NPC;
      regs.regs_NPC += sizeof(md_inst_t);

      /* finish early? */
      if (max_insts && sim_num_insn >= max_insts){
			
			return;
		}
    }
}
