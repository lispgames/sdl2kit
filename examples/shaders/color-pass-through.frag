#version 330

in vec4 interp_color;

out vec4 outputColor;

void main() {
  outputColor = sqrt(interp_color); // simple gamma correction
}
