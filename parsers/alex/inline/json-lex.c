// static so it cannot clash with std/core-extras' kk_vector_from_cintarray when both are linked
static kk_vector_t kk_alex_vector_from_cintarray(kk_intx_t* carray, kk_ssize_t len, kk_context_t* ctx) {
  kk_box_t* array;
  kk_vector_t vec = kk_vector_alloc_uninit(len, &array, ctx);
  for (kk_ssize_t i = 0; i < len; i++) {
    array[i] = kk_integer_box(kk_integer_from_int(carray[i], ctx), ctx);
  }
  return vec;
}
