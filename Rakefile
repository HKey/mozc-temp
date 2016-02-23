require 'rake/clean'

EMACS = ENV['EMACS'] || 'emacs'
CASK  = ENV['CASK'] || 'cask'

COMPILATION_TARGETS = FileList['*.el']

# Utilities

def byte_compile_file(el_file, error_on_warn = false)
  command = %W(#{CASK} exec #{EMACS} -Q -batch)
  command.concat(%w(-l scripts/setup-error-on-warn.el)) if error_on_warn
  command.concat(%W(-f batch-byte-compile #{el_file}))
  sh(*command)
end

# Tasks

CLEAN.include(COMPILATION_TARGETS.ext('.elc'))

rule '.elc' => '.el' do |t|
  byte_compile_file t.source
end

desc 'Compile emacs lisp files'
task compile: COMPILATION_TARGETS.ext('.elc')

desc 'Run compilation test'
task compilation_test: :clean do
  COMPILATION_TARGETS.each do |el_file|
    byte_compile_file el_file, true
  end
end
