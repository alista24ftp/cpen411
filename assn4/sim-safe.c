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
#include <assert.h>

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

// select type of cache:
// 0 - direct mapped instruction cache
// 1 - 4way set associative instruction cache
// 2 - 8way set associative data cache
// 3 - 4way set associative with block prefetching
#define CACHETYPE 3

/*
 * This file implements a functional simulator.  This functional simulator is
 * the simplest, most user-friendly simulator in the simplescalar tool set.
 * Unlike sim-fast, this functional simulator checks for all instruction
 * errors, and the implementation is crafted for clarity rather than speed.
 */

/* simulated registers */
static struct regs_t regs;

/* simulated memory */
static struct mem_t *mem = NULL;

/* track number of refs */
static counter_t sim_num_refs = 0;

/* maximum number of inst's to execute */
static unsigned int max_insts;

// ASSIGNMENT 4:
static counter_t g_icache_miss;
#if CACHETYPE == 2
static counter_t g_dcache_load_miss;
static counter_t g_dcache_store_miss;
static counter_t g_total_load_inst;
static counter_t g_total_store_inst;
static counter_t g_total_writeback;
#endif

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
  stat_reg_counter(sdb, "sim_num_refs",
		   "total number of loads and stores executed",
		   &sim_num_refs, 0, NULL);
  stat_reg_int(sdb, "sim_elapsed_time",
	       "total simulation time in seconds",
	       &sim_elapsed_time, 0, NULL);
  stat_reg_formula(sdb, "sim_inst_rate",
		   "simulation speed (in insts/sec)",
		   "sim_num_insn / sim_elapsed_time", NULL);
  stat_reg_counter(sdb, "sim_num_icache_miss",
		   "total number of instruction cache misses",
		   &g_icache_miss, 0, NULL);
  stat_reg_formula(sdb, "sim_icache_miss_rate",
		   "instruction cache miss rate (percentage)",
		   "100*(sim_num_icache_miss / sim_num_insn)", NULL);
#if CACHETYPE == 2  
  // number of load instructions
  stat_reg_counter(sdb, "sim_num_load_inst",
		   "total number of load instructions",
		   &g_total_load_inst, 0, NULL);
  // number of store instructions
  stat_reg_counter(sdb, "sim_num_store_inst",
		   "total number of store instructions",
		   &g_total_store_inst, 0, NULL);
  // number of writeback events
  stat_reg_counter(sdb, "sim_num_writeback",
		   "total number of writebacks",
		   &g_total_writeback, 0, NULL);
		   
  // total load misses
  stat_reg_counter(sdb, "sim_num_dcache_load_miss",
		   "total number of load cache misses",
		   &g_dcache_load_miss, 0, NULL);
  // total load miss rate
  stat_reg_formula(sdb, "sim_dcache_load_miss_rate",
		   "data cache load miss rate (percentage)",
		   "100*(sim_num_dcache_load_miss / sim_num_load_inst)", NULL);
  // total store misses
  stat_reg_counter(sdb, "sim_num_dcache_store_miss",
		   "total number of store cache misses",
		   &g_dcache_store_miss, 0, NULL);
  // total store miss rate
  stat_reg_formula(sdb, "sim_dcache_store_miss_rate",
		   "data cache store miss rate (percentage)",
		   "100*(sim_num_dcache_store_miss / sim_num_store_inst)", NULL);
  // total writeback rate
  stat_reg_formula(sdb, "sim_writeback_rate",
		   "load/store writeback rate (percentage)",
		   "100*(sim_num_writeback / sim_num_store_inst)", NULL);
#endif
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
  /* nada */
}

/* un-initialize simulator-specific state */
void
sim_uninit(void)
{
  /* nada */
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

#define DNA         (0)

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

/****************** ASSIGNMENT 4 - CREATE STRUCTS **************/
struct block {
	int m_valid; // is block valid?
	md_addr_t m_tag; // tag used to determine whether we have a cache hit
#if CACHETYPE > 0
	int m_timestamp;
#endif
#if CACHETYPE == 2
	int m_dirty;
#endif
};

struct cache {
#if CACHETYPE == 0
	struct block *m_tag_array;
#endif
#if CACHETYPE > 0
	struct block **m_tags;
#endif
	unsigned m_total_blocks;
	unsigned m_set_shift;
	unsigned m_set_mask;
	unsigned m_tag_shift;
#if CACHETYPE > 0
	unsigned m_nways;
#endif
};

// function prototypes:
void cache_access(struct cache *, unsigned, counter_t *);
#if CACHETYPE == 2
void ld_cache_access(struct cache *, unsigned, counter_t *);
void store_cache_access(struct cache *, unsigned, counter_t *);
#endif
#if CACHETYPE == 3
void prefetch_access(struct cache *, unsigned);
#endif

void cache_access( struct cache *c, unsigned addr, counter_t *miss_counter )
{
#if CACHETYPE == 0
	/***************** DIRECT-MAPPED *****************/
	unsigned index, tag;
	index = (addr >> c->m_set_shift) & c->m_set_mask;
	tag = (addr >> c->m_tag_shift);
	assert( index < c->m_total_blocks );
	if(!(c->m_tag_array[index].m_valid && (c->m_tag_array[index].m_tag == tag))) {
		*miss_counter = *miss_counter + 1;
		c->m_tag_array[index].m_valid = 1;
		c->m_tag_array[index].m_tag = tag;
	}
#elif (CACHETYPE == 1 || CACHETYPE == 3)
	/******************* 4-WAY SET ASSOCIATIVE **********/
	unsigned set, tag;
	int i, hits=0, lru=0;
	
	set = (addr >> c->m_set_shift) & c->m_set_mask;
	tag = (addr >> c->m_tag_shift);
	//assert(set < num_sets);
	
	for(i=0; i < c->m_nways; i++){
		if(!c->m_tags[set][i].m_valid){
			*miss_counter = *miss_counter + 1;
			c->m_tags[set][i].m_valid = 1;
			c->m_tags[set][i].m_tag = tag;
			c->m_tags[set][i].m_timestamp = 0;
#if CACHETYPE == 3
			prefetch_access(c, addr + 32);
#endif
			return;
		}
		else{
			c->m_tags[set][i].m_timestamp++;
			if(c->m_tags[set][i].m_timestamp > lru){
				lru = c->m_tags[set][i].m_timestamp;
			}
			if(c->m_tags[set][i].m_tag == tag){
				hits++;
				c->m_tags[set][i].m_timestamp = 0;
				break;
			}
		}
	}
	
	if(hits > 0){
		for(i = i+1; i < c->m_nways; i++){
			if(c->m_tags[set][i].m_valid == 1){
				c->m_tags[set][i].m_timestamp++;
			}
		}
		
	}
	else{
		// is full, so evict LRU block
		*miss_counter = *miss_counter + 1;
		for(i=0; i < c->m_nways; i++){
			if(c->m_tags[set][i].m_timestamp == lru){
				c->m_tags[set][i].m_valid = 1;
				c->m_tags[set][i].m_tag = tag;
				c->m_tags[set][i].m_timestamp = 0;
#if CACHETYPE == 3
				prefetch_access(c, addr + 32);
#endif
				return;
			}
		}
	}
#endif
	

}

#if CACHETYPE == 2
void ld_cache_access(struct cache *c, unsigned addr, counter_t *miss_counter){
	
	unsigned set, tag;
	int i, hits=0, lru=0;
	set = (addr >> c->m_set_shift) & c->m_set_mask;
	tag = (addr >> c->m_tag_shift);
	//assert(set < num_sets);
	
	for(i=0; i < c->m_nways; i++){
		if(!c->m_tags[set][i].m_valid){
			*miss_counter = *miss_counter + 1; // load miss
			// g_total_writeback++; // writeback event occurs upon load miss
			c->m_tags[set][i].m_valid = 1;
			c->m_tags[set][i].m_tag = tag;
			c->m_tags[set][i].m_timestamp = 0;
			c->m_tags[set][i].m_dirty = 0;
			return;
		}
		else{
			c->m_tags[set][i].m_timestamp++;
			if(c->m_tags[set][i].m_timestamp > lru){
				lru = c->m_tags[set][i].m_timestamp;
			}
			if(c->m_tags[set][i].m_tag == tag){
				hits++;
				c->m_tags[set][i].m_timestamp = 0;
				break;
			}
		}
	}
	
	if(hits > 0){
		for(i = i+1; i < c->m_nways; i++){
			if(c->m_tags[set][i].m_valid == 1){
				c->m_tags[set][i].m_timestamp++;
			}
		}
		
	}
	else{
		// is full, so evict LRU block
		*miss_counter = *miss_counter + 1; // load miss
		// g_total_writeback++; // writeback event occurs
		for(i=0; i < c->m_nways; i++){
			if(c->m_tags[set][i].m_timestamp == lru){
				if(c->m_tags[set][i].m_dirty == 1){
					g_total_writeback++;
				}
				c->m_tags[set][i].m_valid = 1;
				c->m_tags[set][i].m_tag = tag;
				c->m_tags[set][i].m_timestamp = 0;
				c->m_tags[set][i].m_dirty = 0;
				return;
			}
		}
	}
}

void store_cache_access(struct cache *c, unsigned addr, counter_t *miss_counter){
	unsigned set, tag;
	int i, hits=0, lru=0;
	set = (addr >> c->m_set_shift) & c->m_set_mask;
	tag = (addr >> c->m_tag_shift);
	//assert(set < num_sets);
	
	// g_total_writeback++; 
	
	for(i=0; i < c->m_nways; i++){
		if(!c->m_tags[set][i].m_valid){
			*miss_counter = *miss_counter + 1; // store miss - cold miss
			// g_total_writeback++;
			c->m_tags[set][i].m_valid = 1;
			c->m_tags[set][i].m_tag = tag;
			c->m_tags[set][i].m_timestamp = 0;
			c->m_tags[set][i].m_dirty = 1;
			return;
		}
		else{
			c->m_tags[set][i].m_timestamp++;
			if(c->m_tags[set][i].m_timestamp > lru){
				lru = c->m_tags[set][i].m_timestamp;
			}
			if(c->m_tags[set][i].m_tag == tag){
				hits++;				
				c->m_tags[set][i].m_timestamp = 0;
				c->m_tags[set][i].m_dirty = 1; // set dirty bit since store modifies this entry
				break;
			}
		}
	}
	
	if(hits > 0){
		for(i = i+1; i < c->m_nways; i++){
			if(c->m_tags[set][i].m_valid == 1){
				c->m_tags[set][i].m_timestamp++;
			}
		}
		
	}
	else{
		// is full, so evict LRU block
		*miss_counter = *miss_counter + 1; // no hits, so store miss
		
		for(i=0; i < c->m_nways; i++){
			if(c->m_tags[set][i].m_timestamp == lru){
				if(c->m_tags[set][i].m_dirty == 1){
					g_total_writeback++;
				}
				c->m_tags[set][i].m_valid = 1;
				c->m_tags[set][i].m_tag = tag;
				c->m_tags[set][i].m_timestamp = 0;
				c->m_tags[set][i].m_dirty = 1;
				return;
			}
		}
	}
}
#endif

#if CACHETYPE == 3
void prefetch_access(struct cache *c, unsigned addr){
	unsigned set, tag;
	int i, lru=0;
	set = (addr >> c->m_set_shift) & c->m_set_mask;
	tag = (addr >> c->m_tag_shift);
	//assert(set < num_sets);
	
	for(i=0; i < c->m_nways; i++){
		if(!c->m_tags[set][i].m_valid){
			
			c->m_tags[set][i].m_valid = 1;
			c->m_tags[set][i].m_tag = tag;
			c->m_tags[set][i].m_timestamp = 0;
			return;
		}
		else{
			//c->m_tags[set][i].m_timestamp++;
			if(c->m_tags[set][i].m_timestamp > lru){
				lru = c->m_tags[set][i].m_timestamp;
			}
			if(c->m_tags[set][i].m_tag == tag){
				// great! a prefetch hit! But we don't really care...
				return;
			}
		}
	}
			
	for(i=0; i < c->m_nways; i++){
		if(c->m_tags[set][i].m_timestamp == lru){
			c->m_tags[set][i].m_valid = 1;
			c->m_tags[set][i].m_tag = tag;
			c->m_tags[set][i].m_timestamp = 0;
			return;
		}
	}
	
}
#endif

/* start simulation, program loaded, processor precise state initialized */
void
sim_main(void)
{
  md_inst_t inst;
  register md_addr_t addr;
  enum md_opcode op;
  register int is_write;
  enum md_fault_type fault;

#if CACHETYPE == 0
  struct cache *icache = (struct cache *) calloc( sizeof(struct cache), 1 );
  icache->m_tag_array = (struct block *) calloc( sizeof(struct block), 512 );
  icache->m_total_blocks = 512;
  icache->m_set_shift = 6;
  icache->m_set_mask = (1 << 9)-1;
  icache->m_tag_shift = 15;
#elif (CACHETYPE == 1 || CACHETYPE == 3)
  struct cache *icache = (struct cache *) calloc( sizeof(struct cache), 1 );
  icache->m_nways = 4;
  icache->m_total_blocks = 1024;
  icache->m_set_shift = 5;
  icache->m_set_mask = (1 << 8)-1;
  icache->m_tag_shift = 13;
  int num_sets = icache->m_total_blocks / icache->m_nways;
  icache->m_tags = (struct block **) calloc( sizeof(struct block *), num_sets );
  int alloc_i = 0;
  for(alloc_i = 0; alloc_i < num_sets; alloc_i++){
	icache->m_tags[alloc_i] = (struct block *) calloc(sizeof(struct block), icache->m_nways);
  }
#else
  struct cache *dcache = (struct cache *) calloc( sizeof(struct cache), 1 );
  dcache->m_nways = 8;
  dcache->m_total_blocks = 256;
  dcache->m_set_shift = 6;
  dcache->m_set_mask = (1 << 5)-1;
  dcache->m_tag_shift = 11;
  int num_sets = dcache->m_total_blocks / dcache->m_nways;
  dcache->m_tags = (struct block **) calloc( sizeof(struct block *), num_sets );
  int alloc_i = 0;
  for(alloc_i = 0; alloc_i < num_sets; alloc_i++){
	dcache->m_tags[alloc_i] = (struct block *) calloc(sizeof(struct block), dcache->m_nways);
  }
#endif  
  fprintf(stderr, "sim: ** starting functional simulation **\n");

  /* set up initial default next PC */
  regs.regs_NPC = regs.regs_PC + sizeof(md_inst_t);


  while (TRUE)
    {
      /* maintain $r0 semantics */
      regs.regs_R[MD_REG_ZERO] = 0;
#ifdef TARGET_ALPHA
      regs.regs_F.d[MD_REG_ZERO] = 0.0;
#endif /* TARGET_ALPHA */

#if CACHETYPE != 2
	// call cache_access()
	cache_access(icache, regs.regs_PC, &g_icache_miss);
#endif

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

      if (MD_OP_FLAGS(op) & F_MEM)
	{
	  sim_num_refs++;
#if CACHETYPE == 2
	  if((MD_OP_FLAGS(op) & F_LOAD) != 0){
		g_total_load_inst++;
		ld_cache_access(dcache, addr, &g_dcache_load_miss);
	  }
	  else if((MD_OP_FLAGS(op) & F_STORE) != 0){
		g_total_store_inst++;
		store_cache_access(dcache, addr, &g_dcache_store_miss);
	  }
#endif
	  if (MD_OP_FLAGS(op) & F_STORE)
	    is_write = TRUE;
	}
	
	

      /* go to the next instruction */
      regs.regs_PC = regs.regs_NPC;
      regs.regs_NPC += sizeof(md_inst_t);

      /* finish early? */
      if (max_insts && sim_num_insn >= max_insts)
	return;
    }
}
