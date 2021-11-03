# This is needed, instead of using config._module.args, because the hostName
# value must be evaluated in modules' imports list, but that would cause
# infinite recursion if config._module.args were used.

with builtins;

elemAt (match "[[:space:]]*([^[:space:]]+)[[:space:]]*" (readFile /etc/hostname)) 0
