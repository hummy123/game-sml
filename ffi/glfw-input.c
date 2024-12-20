#include "export.h"
#include "glad.h"
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

int PRESS = GLFW_PRESS;
int RELEASE = GLFW_RELEASE;

int KEY_S = GLFW_KEY_S;
int KEY_D = GLFW_KEY_D;
int KEY_F = GLFW_KEY_F;

int KEY_J = GLFW_KEY_J;
int KEY_K = GLFW_KEY_K;
int KEY_L = GLFW_KEY_L;

int ARROW_UP = GLFW_KEY_UP;
int ARROW_DOWN = GLFW_KEY_DOWN;
int ARROW_LEFT = GLFW_KEY_LEFT;
int ARROW_RIGHT = GLFW_KEY_RIGHT;

void keyCallback(GLFWwindow* window, int key, int scancode, int action, int mods) {
  mltonKeyCallback(key, scancode, action, mods);
}

void setKeyCallback(GLFWwindow* window) {
  glfwSetKeyCallback(window, keyCallback);
}

void framebufferSizeCallback(GLFWwindow* window, int width, int height) {
  glViewport(0, 0, width, height);
  mltonFramebufferSizeCallback((float) width, (float) height);
}

void setFramebufferSizeCallback(GLFWwindow* window) {
  glfwSetFramebufferSizeCallback(window, framebufferSizeCallback);
}

