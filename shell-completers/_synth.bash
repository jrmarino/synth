#! /usr/bin/env bash

_catport () {
    local PD CR
    PD=$1
    CR=$2
    first=$3
    case "${CR}" in
      "")
        COMPREPLY=( $( cd ${PD}; compgen -W \
        "$(/usr/bin/find * -type d -maxdepth 0 -exec echo {}/ \;)" -- ${CR}))
        ;;
      */*)
        COMPREPLY=( $( cd ${PD}; compgen -W \
        "$(for d in ${CR}*; do [[ -d "$d" ]] && echo $d; done)" -- ${CR}) )
        ;;
      *)
        COMPREPLY=( $( cd ${PD}; compgen -W \
        "$(/usr/bin/find ${CR}* -type d -maxdepth 1 -depth 1 \
          -exec echo {}/ \; 2>/dev/null )" -- ${CR}))
        ;;
    esac
    if [ ${first} -eq 1 ]; then
       if [ -z "${CR}" ]; then
          COMPREPLY=( ${COMPREPLY[@]} $( cd "$(pwd)"; compgen -W \
            "$(find * -type f -depth 0 -maxdepth 0 2>/dev/null)" ))
       else
          COMPREPLY=( ${COMPREPLY[@]} $(compgen -f -- ${CR}))
       fi
    fi
    return 0;
}

_synth () {

    local cur prev opts lopts portsdir
    portsdir=${PORTSDIR:-/usr/ports}
    COMPREPLY=()

    # get command name
    cur="${COMP_WORDS[COMP_CWORD]}"
    
    # get first arguments
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    
    # init opts for first completion
    opts='build configure force just-build install prepare-system
          purge-distfiles rebuild-repository status status-everything
	  test upgrade-system version'
	  
    # switch on second arguments
    case "${prev}" in
	help|configure|version|upgrade-system|prepare-system)
	    return 0 ;;
	status-everything|everything|purge-distfiles|rebuild-repository)
	    return 0 ;;
	status|build|just-build|install|force|test)
	_catport ${portsdir} "${cur}" 1
	return 0 ;;
    esac
    if [ -d "${portsdir}/${prev}" ]; then
	_catport ${portsdir} "${cur}" 0
	return 0;
    fi

    if [ ${COMP_CWORD} -eq 1 ]; then
       # display commands
       COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
    fi
}

complete -F _synth synth
