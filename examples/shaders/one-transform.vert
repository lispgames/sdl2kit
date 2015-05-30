// perform a single transformation

#version 330

layout(location = 0) in vec4 position;

uniform mat4 model_to_clip;

void main () {
  gl_Position = model_to_clip * position;
}
