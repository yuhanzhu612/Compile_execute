#ifndef __COMMON_H__
#define __COMMON_H__

#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#include <generated/autoconf.h>
#include <macro.h>

#if CONFIG_MBASE + CONFIG_MSIZE > 0x100000000ul
#define PMEM64 1
#endif

typedef MUXDEF(CONFIG_ISA64, uint64_t, uint32_t) word_t;
typedef MUXDEF(CONFIG_ISA64, int64_t, int32_t)  sword_t;
#define FMT_WORD MUXDEF(CONFIG_ISA64, "0x%016lx", "0x%08x")

typedef word_t rtlreg_t;
typedef word_t vaddr_t;
typedef MUXDEF(PMEM64, uint64_t, uint32_t) paddr_t;
#define FMT_PADDR MUXDEF(PMEM64, "0x%016lx", "0x%08x")
typedef uint16_t ioaddr_t;

#define CP printf("%s: %d\n", __FILE__, __LINE__);fflush( stdout );
struct DynamicConfig {
  bool ignore_illegal_mem_access;
  bool debug_difftest;
};
extern struct DynamicConfig dynamic_config;
void update_dynamic_config(void* config);

#include <debug.h>

#endif
