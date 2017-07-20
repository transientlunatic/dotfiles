weather (){
    if [ -z "$1" ]; then
	loc="Glasgow"
    else
	loc=$1
    fi
       
    curl http://wttr.in/$loc
}
