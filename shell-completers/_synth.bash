#! /usr/bin/env bash

_synth () {

    local cur prev opts lopts
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
	    COMPREPLY=('     space-delimited list of port origins'
	               '-or- path to file containing one port origin per line')
	    return 0 ;;
    esac

    # if doesn't exist, return opts
    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
}

complete -F _synth synth
