#version 330

// simple fragment shader assigning the same outputColor on all fragments
out vec4 outputColor;

uniform vec4 color;

void main() {
  outputColor = sqrt(color); // simple gamma correction
}
