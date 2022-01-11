//
//  kunvg.c
//  nvgdemo
//
//  Created by Mohsen Agsen on 1/7/22.
//

#include "kunvg.h"

NVGcontext *nvgContextFor(const char *key, kunvobj *target);

static inline NVGcolor NVGColorFromInt(uint64_t rgba) {
  uint64_t r = (rgba >> 24) & 0xff;
  uint64_t g = (rgba >> 16) & 0xff;
  uint64_t b = (rgba >> 8) & 0xff;
  uint64_t a = rgba & 0xff;
  return nvgRGBA(r, g, b, a);
}

static uint64_t NVGcolorToInt(NVGcolor color) {
  uint64_t r = 255.0 * color.r;
  uint64_t g = 255.0 * color.g;
  uint64_t b = 255.0 * color.b;
  uint64_t a = 255.0 * color.a;
  uint64_t rgba = r << 24 | g << 16 | b << 8 | a;
  return rgba;
}

static void NVGpaintToArray(kuvm *vm, NVGpaint *p, kuaobj *arr) {
  ku_arrset(vm, arr, 0, NUM_VAL(p->xform[0]));
  ku_arrset(vm, arr, 1, NUM_VAL(p->xform[1]));
  ku_arrset(vm, arr, 2, NUM_VAL(p->xform[2]));
  ku_arrset(vm, arr, 3, NUM_VAL(p->xform[3]));
  ku_arrset(vm, arr, 4, NUM_VAL(p->xform[4]));
  ku_arrset(vm, arr, 5, NUM_VAL(p->xform[5]));
  ku_arrset(vm, arr, 6, NUM_VAL(p->extent[0]));
  ku_arrset(vm, arr, 7, NUM_VAL(p->extent[1]));
  ku_arrset(vm, arr, 8, NUM_VAL(p->radius));
  ku_arrset(vm, arr, 9, NUM_VAL(p->feather));
  ku_arrset(vm, arr, 10, NUM_VAL(NVGcolorToInt(p->innerColor)));
  ku_arrset(vm, arr, 11, NUM_VAL(NVGcolorToInt(p->outerColor)));
  ku_arrset(vm, arr, 12, NUM_VAL(p->image));
}

static void NVGpaintFromArray(kuvm *vm, NVGpaint *p, kuaobj *arr) {
  p->xform[0] = AS_NUM(ku_arrget(vm, arr, 0));
  p->xform[1] = AS_NUM(ku_arrget(vm, arr, 1));
  p->xform[2] = AS_NUM(ku_arrget(vm, arr, 2));
  p->xform[3] = AS_NUM(ku_arrget(vm, arr, 3));
  p->xform[4] = AS_NUM(ku_arrget(vm, arr, 4));
  p->xform[5] = AS_NUM(ku_arrget(vm, arr, 5));
  
  p->extent[0] = AS_NUM(ku_arrget(vm, arr, 6));
  p->extent[1] = AS_NUM(ku_arrget(vm, arr, 7));

  p->radius = AS_NUM(ku_arrget(vm, arr, 8));
  p->feather = AS_NUM(ku_arrget(vm, arr, 9));
  
  p->innerColor = NVGColorFromInt(AS_NUM(ku_arrget(vm, arr, 10)));
  p->outerColor = NVGColorFromInt(AS_NUM(ku_arrget(vm, arr, 11)));
  p->image = (int)AS_NUM(ku_arrget(vm, arr, 12));
}
kuval nanovg_cons(kuvm *vm, int argc, kuval *argv) {
  if (argc < 2 || !IS_INSTANCE(argv[0]) || !IS_STR(argv[1])) {
    ku_err(vm, "expected nanovg(object, key)");
    return NIL_VAL;
  }
  kunvobj *no = (kunvobj*)ku_objalloc(vm, sizeof(kunvobj), OBJ_CINST);
  no->target = argv[0];
  no->ctx = nvgContextFor(AS_STR(argv[1])->chars, no);
  no->method = ku_strfrom(vm, "render", 6);
  return OBJ_VAL(no);
}

static inline uint64_t nanovg_args2intcolor(kuval *argv) {
  uint64_t r = (uint64_t)AS_NUM(argv[0]);
  uint64_t g = (uint64_t)AS_NUM(argv[1]);
  uint64_t b = (uint64_t)AS_NUM(argv[2]);
  uint64_t a = (uint64_t)AS_NUM(argv[3]);
  uint64_t rgba = r << 24 | g << 16 | b << 8 | a;
  return rgba;
}


#define AN(i) AS_NUM(argv[i])

kuval nanovg_icall(kuvm *vm, kuobj *o, kustr *m, int argc, kuval *argv) {
  kunvobj *no = (kunvobj*)o;
  
  if (strcmp(m->chars, "beginFrame") == 0 && argc == 3) {
    double w = AS_NUM(argv[0]);
    double h = AS_NUM(argv[1]);
    double s = AS_NUM(argv[2]);
    nvgBeginFrame(no->ctx, w, h, s);
  } else if (strcmp(m->chars, "endFrame") == 0) {
    nvgEndFrame(no->ctx);
  } else if (strcmp(m->chars, "beginPath") == 0) {
    nvgBeginPath(no->ctx);
  } else if (strcmp(m->chars, "rect") == 0 && argc == 4) {
    double x = AS_NUM(argv[0]);
    double y = AS_NUM(argv[1]);
    double w = AS_NUM(argv[2]);
    double h = AS_NUM(argv[3]);
    nvgRect(no->ctx, x, y, w, h);
  } else if (strcmp(m->chars, "fillColor") == 0 && argc == 1) {
    nvgFillColor(no->ctx, NVGColorFromInt((uint64_t)AS_NUM(argv[0])));
  } else if (strcmp(m->chars, "rgba") == 0 && argc == 4) {
    return NUM_VAL(nanovg_args2intcolor(argv));
  } else if (strcmp(m->chars, "fill") == 0) {
    nvgFill(no->ctx);
  } else if (strcmp(m->chars, "translate") == 0 && argc == 2) {
    nvgTranslate(no->ctx, AS_NUM(argv[0]), AS_NUM(argv[1]));
  } else if (strcmp(m->chars, "rotate") == 0 && argc == 1) {
    nvgRotate(no->ctx, AS_NUM(argv[0]));
  } else if (strcmp(m->chars, "resetTransform") == 0 && argc == 0) {
    nvgResetTransform(no->ctx);
  } else if (strcmp(m->chars, "strokeColor") == 0 && argc == 1) {
    nvgStrokeColor(no->ctx, NVGColorFromInt((uint64_t)AS_NUM(argv[0])));
  } else if (strcmp(m->chars, "strokeWidth") == 0 && argc == 1) {
    nvgStrokeWidth(no->ctx, AS_NUM(argv[0]));
  } else if (strcmp(m->chars, "moveTo") == 0 && argc == 2) {
    nvgMoveTo(no->ctx, AS_NUM(argv[0]), AS_NUM(argv[1]));
  } else if (strcmp(m->chars, "lineTo") == 0 && argc == 2) {
    nvgLineTo(no->ctx, AS_NUM(argv[0]), AS_NUM(argv[1]));
  } else if (strcmp(m->chars, "arc") == 0 && argc == 6) {
    double cx = AS_NUM(argv[0]);
    double cy = AS_NUM(argv[1]);
    double r = AS_NUM(argv[2]);
    double a0 = AS_NUM(argv[3]);
    double a1 = AS_NUM(argv[4]);
    int dir = (int)AS_NUM(argv[5]);
    nvgArc(no->ctx, cx, cy, r, a0, a1, dir);
  } else if (strcmp(m->chars, "stroke") == 0 && argc == 0) {
    nvgStroke(no->ctx);
  } else if (strcmp(m->chars, "circle") == 0 && argc == 3) {
    double cx = AS_NUM(argv[0]);
    double cy = AS_NUM(argv[1]);
    double r = AS_NUM(argv[2]);
    nvgCircle(no->ctx, cx, cy, r);
  } else if (strcmp(m->chars, "createFont") == 0 && argc == 2) {
    const char *name = AS_STR(argv[0])->chars;
    const char *fileName = AS_STR(argv[1])->chars;
    int i = nvgCreateFont(no->ctx, name, fileName);
    return NUM_VAL(i);
  }  else if (strcmp(m->chars, "fontFaceId") == 0 && argc == 1) {
    nvgFontFaceId(no->ctx, (int)AS_NUM(argv[0]));
  } else if (strcmp(m->chars, "textAlign") == 0 && argc == 1) {
    nvgTextAlign(no->ctx, (int)AS_NUM(argv[0]));
  } else if (strcmp(m->chars, "currentTransform") == 0 && argc == 1) {
    if (IS_ARRAY(argv[0])) {
      float xform[6];
      nvgCurrentTransform(no->ctx, xform);
      kuaobj *ao = AS_ARRAY(argv[0]);
      for (int i = 0; i < 6; i++) {
        ku_arrset(vm, ao, i, NUM_VAL(xform[i]));
      }
    } else {
      ku_err(vm, "array expected");
    }
  } else if (strcmp(m->chars, "transform") == 0 && argc == 6) {
    nvgTransform(no->ctx, AS_NUM(argv[0]), AS_NUM(argv[1]), AS_NUM(argv[2]), AS_NUM(argv[3]), AS_NUM(argv[4]), AS_NUM(argv[5]));
  } else if (strcmp(m->chars, "fontSize") == 0 && argc == 1) {
    nvgFontSize(no->ctx, AS_NUM(argv[0]));
  } else if (strcmp(m->chars, "text") == 0 && argc == 3) {
    nvgText(no->ctx, AS_NUM(argv[0]), AS_NUM(argv[1]), AS_STR(argv[2])->chars, NULL);
  } else if (strcmp(m->chars, "closePath") == 0 && argc == 0) {
    nvgClosePath(no->ctx);
  } else if (strcmp(m->chars, "ellipse") == 0 && argc == 4) {
    nvgEllipse(no->ctx, AS_NUM(argv[0]), AS_NUM(argv[1]), AS_NUM(argv[2]), AS_NUM(argv[3]));
  }  else if (strcmp(m->chars, "scale") == 0 && argc == 2) {
    nvgScale(no->ctx, AS_NUM(argv[0]), AS_NUM(argv[1]));
  } else if (strcmp(m->chars, "linearGradient") == 0 && argc == 7 && IS_ARRAY(argv[6])) {
    double sx = AN(0), sy = AN(1), ex = AN(2), ey = AN(3);
    NVGcolor icolor = NVGColorFromInt((uint64_t)AN(4));
    NVGcolor ocolor = NVGColorFromInt((uint64_t)AN(5));    
    kuaobj *arr = AS_ARRAY(argv[6]);
    NVGpaint p = nvgLinearGradient(no->ctx, sx, sy, ex, ey, icolor, ocolor);
    NVGpaintToArray(vm, &p, arr);
  } else if (strcmp(m->chars, "fillPaint") == 0 && argc == 1 && IS_ARRAY(argv[0])) {
    NVGpaint paint;
    NVGpaintFromArray(vm, &paint, AS_ARRAY(argv[0]));
    nvgFillPaint(no->ctx, paint);
  } else if (strcmp(m->chars, "radialGradient") == 0 && argc == 7 && IS_ARRAY(argv[6])) {
    double cx = AN(0), cy = AN(1), inr = AN(2), outr = AN(3);
    NVGcolor icol = NVGColorFromInt((uint64_t)AN(4));
    NVGcolor ocol = NVGColorFromInt((uint64_t)AN(5));
    kuaobj *arr = AS_ARRAY(argv[6]);
    NVGpaint p = nvgRadialGradient(no->ctx, cx, cy, inr, outr, icol, ocol);
    NVGpaintToArray(vm, &p, arr);
  }
  else {
    ku_err(vm, "unexpected method %s\n", m->chars);
  }
  return NIL_VAL;
}

kuval nanovg_sget(kuvm *vm, kustr *p) {
  if (strcmp(p->chars, "CCW") == 0) {
    return NUM_VAL(NVG_CCW);
  } else if (strcmp(p->chars, "CW") == 0) {
    return NUM_VAL(NVG_CW);
  } else if (strcmp(p->chars, "ALIGN_CENTER") == 0) {
    return NUM_VAL(NVG_ALIGN_CENTER);
  } else if (strcmp(p->chars, "ALIGN_LEFT") == 0) {
    return NUM_VAL(NVG_ALIGN_LEFT);
  } else if (strcmp(p->chars, "ALIGN_RIGHT") == 0) {
    return NUM_VAL(NVG_ALIGN_RIGHT);
  } else if (strcmp(p->chars, "ALIGN_MIDDLE") == 0) {
    return NUM_VAL(NVG_ALIGN_MIDDLE);
  } else if (strcmp(p->chars, "ALIGN_TOP") == 0) {
    return NUM_VAL(NVG_ALIGN_TOP);
  } else if (strcmp(p->chars, "ALIGN_BOTTOM") == 0) {
    return NUM_VAL(NVG_ALIGN_BOTTOM);
  }
  return NIL_VAL;
}

kuval nanovg_ifree(kuvm *vm, kuobj *o) {
  kunvobj *no = (kunvobj*)o;
  nvgDeleteInternal(no->ctx);
  no->ctx = NULL;
  ku_alloc(vm, no, sizeof(kunvobj), 0);
  return NIL_VAL;
}

kuval nanovg_imark(kuvm *vm, kuobj *o) {
  kunvobj *no = (kunvobj*)o;
  ku_markobj(vm, AS_OBJ(no->target));
  return NIL_VAL;
}

void kuvg_reg(kuvm *vm) {
  kucclass *cc = ku_cclassnew(vm, "nanovg");
  cc->cons = nanovg_cons;
  cc->sget = nanovg_sget;
  cc->icall = nanovg_icall;
  cc->ifree = nanovg_ifree;
  cc->imark = nanovg_imark;
  ku_cclassdef(vm, cc);
}

void kuvg_render(kuvm *vm, kunvobj *obj, double width, double height, double scale) {
  if (obj == NULL) {
    return;
  }
  bool native;
  kuval *oldsp = vm->sp;
  ku_push(vm, obj->target);
  ku_push(vm, OBJ_VAL(obj));
  ku_push(vm, NUM_VAL(width));
  ku_push(vm, NUM_VAL(height));
  ku_push(vm, NUM_VAL(scale));
  int oldbase = vm->baseframe;
  vm->baseframe = vm->framecount;
  if (ku_invoke(vm, obj->method, 4, &native)) {
    ku_run(vm);
  }
  vm->baseframe = oldbase;
  vm->sp = oldsp;
}
