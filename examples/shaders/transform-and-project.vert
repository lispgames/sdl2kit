// perform a single transformation and set up for perspective divide

#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec4 color;

uniform mat4 model_to_clip;
uniform mat4 perspective_matrix;

smooth out vec4 interp_color;

void main () {
  interp_color = color;
  gl_Position = perspective_matrix * model_to_clip * position;
}
