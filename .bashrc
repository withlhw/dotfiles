# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# hyungwooklee
if [[ ! $TERM =~ screen ]]; then
  exec tmux
fi

# if [ -f `which powerline-daemon` ]; then
#     powerline-daemon -q
#     POWERLINE_BASH_CONTINUATION=1
#     POWERLINE_BASH_SELECT=1
#     . /usr/share/powerline/bindings/bash/powerline.sh
# fi

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

# adjust mouse speed
xinput -set-prop 10 'Device Accel Constant Deceleration' 1.8
xinput -set-prop 10 'Evdev Scrolling Distance' 1 2 8

alias aglsetup='source meta-agl/scripts/aglsetup.sh -m raspberrypi4 agl-demo agl-netboot agl-appfw-smack agl-html5-framework'
alias aglbuild='bitbake agl-demo-platform'
alias aglflash='sudo bmaptool copy tmp/deploy/images/raspberrypi4/agl-demo-platform-raspberrypi4.wic.xz /dev/sde'
alias cbuild='ninja -j100 -C out/Release content_shell'
alias cdebugjava='out/Release/bin/content_shell_apk run --wait-for-java-debugger'
alias cdebuggdb='gdb --tui --args out/Release/content_shell --single-process'
alias cflash='./out/Release/bin/content_shell_apk run'
alias cstack='./out/Release/bin/stack_content_shell_apk'
alias clayout='python third_party/blink/tools/run_web_tests.py -t Release'
alias crbuild='ninja -j100 -C out/Release chrome_public_apk'
alias crflash='./out/Release/bin/chrome_public_apk run'
alias crstack='./out/Release/bin/stack_chrome_public_apk'
alias crdebugchrome='./build/android/adb_gdb_chrome_public --output-directory=out/Release'
alias crdebugjava='out/Release/bin/chrome_public_apk run --wait-for-java-debugger'
alias crdebuggdb='out/Release/bin/chrome_public_apk gdb --ide'
alias crclang='/usr/lib/icecc/icecc-create-env --clang /home/hyungwooklee/Projects2/chromium/src/third_party/llvm-build/Release+Asserts/bin/clang /usr/lib/icecc/compilerwrapper'
alias crgradle='build/android/gradle/generate_gradle.py --output-directory out/Release --project-dir out/Release/gradle --target //chrome/android:chrome_public_apk'
alias crossetup='cros_sdk --chrome_root=/mnt/sdb1/naverwhale3'
alias crosbuild='./build_packages --board=${BOARD}'
alias hbuild='autoninja -C out/Release content_shell_apk'
alias hflash='./out/Release/bin/content_shell_apk run'
alias hremote='adb forward tcp:9222 localabstract:content_shell_devtools_remote'
alias hsudoadb='sudo ~/Workspace/android-sdks/platform-tools/adb start-server'
alias htracing='./run_measurement --browser=android-content-shell --allow-live-sites --profiler=trace loading_trace page_sets/naver.json'
alias hsoflash='adb push out/Release/content_shell_apk/libs/armeabi-v7a/libcontent_shell_content_view.so /data/data/com.nhn.android.search/files/webkit/libhiggs_web_engine.so'
alias henvsetup='source build/android/envsetup.sh'
alias hsoaddr2line='third_party/android_tools/ndk/toolchains/arm-linux-androideabi-4.8/prebuilt/linux-x86_64/bin/arm-linux-androideabi-addr2line -C -f -e out/Release/lib/libxwalkcore.so'
alias hsocrashdmp0='java -jar ~/Workspace/apktool/dumptool.jar'
alias hsocrashdmp1='dmp2minidump.py'
alias hsocrashdmp2='minidump_stackwalk'
alias hsocrashdmp3='higgs-addr2line.py'
alias hsocrashdmpa='higgs-nelo2autogen.py'
alias hdebugnaver0='./build/android/adb_gdb_naverapp --start --output-directory=out/Default'
alias hdebugnaver1='./build/android/adb_gdb_naverapp --sandboxed=0 --output-directory=out/Default'
alias hdebugshell0='./build/android/adb_gdb_content_shell --start --output-directory=out/Default'
alias hdebugshell1='./build/android/adb_gdb_content_shell --sandboxed=0 --output-directory=out/Default'
alias hlayouttest='./run-webkit-tests --android --no-pixel --no-retry-failures .'
alias hawbuild='autoninja -C out/Release system_webview_shell_apk'
alias hawflash='adb install -r out/Release/apks/SystemWebViewShell.apk'
alias hawdebug='./build/android/adb_gdb_android_webview_shell'
alias hswbuild='autoninja -C out/Release system_webview_apk'
alias hswflash='adb install -r -d out/Release/apks/SystemWebView.apk'
alias sbuild='./Tools/Scripts/build-webkit --sling --release'
alias sflash='adb install -r Tools/MiniBrowser/droid/bin/MiniBrowser.apk'
alias waglsetup='source meta-agl/scripts/aglsetup.sh -m qemux86-64 agl-whale-demo agl-devel browser'
alias waglsetup2='source meta-agl/scripts/aglsetup.sh -m raspberrypi4 agl-whale-demo agl-devel browser'
alias waglbuild='bitbake agl-whale-demo-platform'
alias waglflash='sudo qemu-system-x86_64 -enable-kvm -m 2048 -hda ../build/tmp/deploy/images/qemux86-64/agl-whale-demo-platform-qemux86-64.wic.vmdk -net user,hostfwd=tcp::10022-:22 -net nic'
alias waglconsole='ssh-keygen -f "/home/hyungwooklee/.ssh/known_hosts" -R [localhost]:10022 & ssh root@localhost -p 10022'
alias wcrossetup='cros chrome-sdk --board=${BOARD} --log-level=info --nogoma'
alias wcrossetup2='cros chrome-sdk --board=${BOARD} --download-vm --clear-sdk-cache --log-level=info --nogoma'
alias wcrosbuild='autoninja -C out_${BOARD}/Release chrome nacl_helper'
alias wcrosvmstart='cros_vm --start'
alias wcrosflash='deploy_chrome --build-dir=out_${BOARD}/Release --to=localhost --port=9222'
alias wcrosvmviewer='vncviewer localhost:5900 &'
alias wenvsetup='source build/android/envsetup.sh'
alias wbuild='ninja -j100 -C out/default whale_public_apk'
alias wflash='./out/default/bin/chrome_public_apk run'
alias wdebugvside='./out/default/bin/chrome_public_apk gdb --ide'
alias wgradle='build/android/gradle/generate_gradle.py --output-directory out/default --project-dir out/default/gradle --target //chrome/android:chrome_public_apk'
#alias wgradle='build/android/gradle/generate_gradle.py --output-directory out/default --project-dir out/default/gradle --target //whale/whale/android:whale_public_apk --split-projects --use-gradle-process-resources'
alias wdebugwhale0='./build/android/adb_gdb_whale_public'
alias wdebugwhale1='./build/android/adb_gdb_whale_public --sandboxed=0'
alias wsoaddr2line='./third_party/android_ndk/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/arm-linux-androideabi-addr2line -C -f -e out/default/lib.unstripped/libwhale.so'
alias wstack='./out/default/bin/stack_chrome_public_apk'
alias xbuild='ninja -j100 -C out/Release xwhale_android_library xwhale_instrumentation_apk && cp -r out/Release/xwhale_android_library/libs ~/Projects/naverapp/Search/lib/crosswalkwebview'
alias xbuild2='autoninja -C out/Release xwalk_core_library xwalk_core_shell_apk && cp -r out/Release/xwalk_core_library/libs ~/Projects/naverapp/Search/lib/crosswalkwebview'
alias xdebugjava='out/Release/bin/xwalk_core_shell_apk run --wait-for-java-debugger'
alias xsocrashdmp0='java -jar ~/Workspace/apktool/dumptool.jar'
alias xsocrashdmp1='dmp2minidump.py'
alias xsocrashdmp2='minidump_stackwalk'
alias xsocrashdmp3='xsoaddr2line'
alias xsocrashdmpa='xwalk-nelo2autogen.py'
alias xdebugshell='./xwalk/build/android/adb_gdb_xwalk_core_shell --output-directory=out/Release'
alias xsoaddr2line='/home/hyungwooklee/Projects/naverxwalk/src/third_party/./android_ndk/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/arm-linux-androideabi-addr2line -C -f -e out/Release/lib.unstripped/libxwalkcore.so'
alias xstack='./out/Release/bin/stack_xwhale_instrumentation_apk'
alias xflash='out/Release/bin/xwhale_instrumentation_apk run'
alias gtags='find . -type f -name '*.cc' -o -name '*.mm' -o -name '*.h' | gtags -f -'
alias stags='ctags --languages=C++ --exclude=third_party --exclude=.git --exclude=build --exclude=out -R -f .tags'

export PATH=/usr/lib/icecc/bin:$HOME/Projects2/chromium/src/third_party/llvm-build/Release+Asserts/bin:$PATH:$HOME/Workspace/android-sdks/tools:$HOME/Workspace/android-sdks/platform-tools:$HOME/Workspace/depot_tools:$HOME/Workspace/apktool:$HOME/Workspace/dex2jar
#export PATH=$PATH:$HOME/Workspace/android-sdks/tools:$HOME/Workspace/android-sdks/platform-tools:$HOME/Workspace/depot_tools:$HOME/Workspace/apktool:$HOME/Workspace/dex2jar
export PS1="\u@\h \[\033[32m\]\w\[\033[33m\]\$(parse_git_branch)\[\033[00m\] $ "
#export ANDROID_NDK_ROOT=$HOME/Workspace/android-ndk-r8c
#export ANDROID_SDK_HOME=$HOME/Workspace/android-sdks
export CCACHE_PREFIX=icecc
export USE_CCACHE=1
export TERM="screen-256color"
export CHANGE_LOG_NAME="Hyungwook Lee"
export CHANGE_LOG_EMAIL_ADDRESS=hyungwook.lee@navercorp.com
export CHROME_DEVEL_SANDBOX=/usr/local/sbin/chrome-devel-sandbox
export ENVSETUP_GYP_CHROME_SRC=$HOME/Projects/naverxwalk/src
#export CHROMIUM_OUTPUT_DIR=out/default
#export GYP_DEFINES="werror="
export GYP_GENERATORS='ninja'
export USE_SCHEDULER=10.77.12.69
export ICECC_CLANG_REMOTE_CPP=1
export ICECC_VERSION=$HOME/Workspace/m83clang.tar.gz
export BOARD=amd64-generic
export XWALK_OS_ANDROID=1

