# meson.build
project('OuiWisp', 'c',
  version: '0.1',
  default_options: ['warning_level=3', 'c_std=c23']
)

sources = [
  'src/lexer.c',
  'src/parser.c',
  'src/ggc.c'
]

executable('OuiWisp', 
  sources,
  install: true,
  c_args: ['-g3']
)
