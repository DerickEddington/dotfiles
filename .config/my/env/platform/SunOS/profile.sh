if [ "${HOME-}" ]; then  # (Just in case.)

    if [ -f "${MY_CONFIG_HOME-}"/my/htop/platform/"${MY_PLATFORM_OS-}"/htoprc ]; then
        export HTOPRC=$MY_CONFIG_HOME/my/htop/platform/$MY_PLATFORM_OS/htoprc
    fi

fi
