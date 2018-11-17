;;; core/cli/debug.el -*- lexical-binding: t; -*-

(dispatcher! info (nucleus/info)
  "Output system info in markdown for bug reports.")

(dispatcher! (version v) (nucleus/version)
  "Reports the version of Doom and Emacs.")
