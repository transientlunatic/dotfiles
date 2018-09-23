weather (){
    if [ -z "$1" ]; then
	loc="Glasgow"
    else
	loc=$1
    fi
       
    curl http://wttr.in/$loc
}

moon () {
    # Print the phase of the moon with a nice ASCII art representation of the phase.
     curl http://wttr.in/Moon
}
