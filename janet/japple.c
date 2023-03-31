#include <janet.h>

static Janet tyof_j(int32_t argc, Janet *argv) {
  janet_fixarity(argc, 0);
  return janet_wrap_nil();
}

static JanetReg cfuns[] = {
    {"tyof", tyof_j, "type of expression"},
    {NULL, NULL, NULL}
};

// janet_wrap_integer
JANET_MODULE_ENTRY(JanetTable *env) {
    janet_cfuns(env, "apple", cfuns);
}
