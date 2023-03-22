#!/bin/bash
# LICENSE: PUBLIC DOMAIN
# switch between my layouts

# If an explicit layout is provided as an argument, use it. Othwerwise, select the next layout from the set [pl, us].
if [[ -n "$1" ]]; then
    setxkbmap $1
else
    layout=$(setxkbmap -query | awk '/^layout/{print $2}')
    case $layout in
	pl)
	    setxkbmap us
	    ;;
	us)
	    setxkbmap pl
	    ;;
	*)
	    setxkbmap pl
	    ;;
    esac
fi

