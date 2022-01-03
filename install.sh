#!/bin/sh
guile_site_dir="$(guile -c '(display (%site-dir))')"
echo "Making directory $guile_site_dir/language"
mkdir -p "$guile_site_dir/language"
ln -vs "$PWD" "$guile_site_dir/language/nib"

# test that things are working and compile files
guile --auto-compile -c '(use-modules (language nib spec))'
