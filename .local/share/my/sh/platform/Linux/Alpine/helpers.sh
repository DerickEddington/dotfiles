# Platform-specific identification
#
MY_PLATFORM_VERSION=${MY_PLATFORM_VERSION%.*}

# For bootstrapping my setup independently of my other more-involved package-installing modules.
#
_my_install_bash()                      { sudo apk add bash ;}
_my_install_git()                       { sudo apk add git ;}
