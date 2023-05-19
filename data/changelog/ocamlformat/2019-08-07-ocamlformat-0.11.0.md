---
title: Ocamlformat 0.11.0
date: "2019-08-07"
tags: [ocamlformat, platform, release]
changelog: |
  - Improve: generalize API of Config_option (#952, @gpetiot)
  - Improve: new 'before' value for option 'sequence-style' (#947, @gpetiot)
  - Project: create issue templates (#950, @gpetiot)
  - Improve: tidying up Conf.ml (#951, @gpetiot)
  - Improve: parse code in comments (#934, @gpetiot)
  - Fix comments' placement (do not look at loc_stack) (#923, @gpetiot)
  - Doc: setting flags in .ocamlformat (#944, @gpetiot)
  - Doc: enable-outside-detected-project necessary for global conf file (#948, @gpetiot)
  - Fix hashbang handling (#946, @hhugo)
  - Improve: support Shell-style regular expressions in .ocamlformat-ignore and .ocamlformat-enable files (#937, @gpetiot)
  - Improve: force break after an infix op only if followed by another one (#935, @gpetiot)
  - Fix break-separators=after-and-docked for lists and arrays (#931, @gpetiot)
  - Improve: deprecate option break-string-literals and change its default value (#932, @gpetiot)
  - Improve: break with labeled arrow type (#933, @gpetiot)
  - Improve: disambiguate non-breaking matching structures (#857, @gpetiot)
  - Improve: warning 50 handled like an internal error (#930, @gpetiot)
  - Fix break-separators=after-and-docked for record patterns (#929, @gpetiot)
  - Fix closing parenthesis indentation when on separate line (#928, @gpetiot)
  - Improve: split the Conf.ml file (#920, @gpetiot)
  - Fix position of comments after anonymous functions (#919, @gpetiot)
  - Fix: comments around disabled block (#918, @hhugo)
  - Fix monadic bindings (new 4.08 syntax) (#911, @gpetiot)
  - Fix attribute when break-infix-before-func=false (#916, @gpetiot)
  - Improve: update ocamlformat_reason opam file to 2.0 format (#913, @avsm)
  - Fix attributes of modules (#910, @gpetiot)
  - Fix docstrings of exceptions (#909, @gpetiot)
  - Fix attribute location in Normalization (#908, @gpetiot)
  - Improve: add the 'ocamlformat-file-kind' argument to the emacs hook (#905, @gpetiot)
  - Improve: dunify testsuite (#881, @trRefis)
  - Improve: add trailing semicolon inside record when break-separators=after-and-docked (#899, @gpetiot)
  - Fix compilation with 4.06 and 4.07 (#898, @gpetiot)
  - Improve: add a new way to indicate multiline delimiters (#876, @trefis)
  - Fix inconsistency of break-separators=after-and-docked for record expressions (#856, @gpetiot)
---
