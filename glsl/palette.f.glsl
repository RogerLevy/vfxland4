uniform sampler2D al_tex;
uniform sampler2D pal_tex;
uniform float pal_ofs;
varying vec4 varying_color;
varying vec2 varying_texcoord;

void main()
{
    float index = texture2D(al_tex, varying_texcoord).r;
    if (index == 0.0) discard;
    if (index == 1.0) discard;
    vec4 color = texture2D( pal_tex, vec2( ( index + pal_ofs ) / 8, 0 ) );
    gl_FragColor = color;
}