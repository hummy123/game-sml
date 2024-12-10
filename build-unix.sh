#!/bin/sh
mlton -link-opt "$(pkg-config --cflags glfw3) $(pkg-config --static --libs glfw3)" \
  -export-header ffi/export.h \
  oms.mlb \
  ffi/glad.c \
  ffi/glfw-export.c \
  ffi/gles3-export.c \
  ffi/glfw-input.c
