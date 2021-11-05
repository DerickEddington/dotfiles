# This is needed, instead of using config.my, because the hostName value must be
# evaluated in modules' imports list, but that would cause infinite recursion if
# config.my were used.  Having this in an imported file allows reuse.

with builtins;

elemAt (match "[[:space:]]*([^[:space:]]+)[[:space:]]*" (readFile /etc/hostname)) 0
