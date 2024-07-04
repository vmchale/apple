#include<janet.h>
#include<HsFFI.h>
#include"../include/apple.h"

static Janet tyof_j(int32_t argc, Janet *argv) {
  janet_fixarity(argc, 1);
  janet_checktypes(argv[0], JANET_TFLAG_STRING);
  const uint8_t* inp=janet_unwrap_string(argv[0]);
  char** e;
  char* o=apple_printty((const char*) inp, e);
  return janet_wrap_string((uint8_t*)o);
}

static JanetReg cfuns[] = {
    {"tyof", tyof_j, "type of expression"},
    {NULL, NULL, NULL}
};

JANET_MODULE_ENTRY(JanetTable *env) {
    hs_init(0,0)
    janet_cfuns(env, "apple", cfuns);
}
