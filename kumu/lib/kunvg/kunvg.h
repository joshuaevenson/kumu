
#ifndef kunvg_h
#define kunvg_h

#include <stdio.h>
#include "kumu.h"
#include "nanovg.h"

typedef struct {
  kunobj base;
  kuval target;
  NVGcontext *ctx;
  kustr *method;
} kunvobj;

void kuvg_reg(kuvm *vm);
void kuvg_render(kuvm *vm, kunvobj *obj, double width, double height, double scale);

#endif