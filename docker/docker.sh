function dockerenv {
    container=$1
    shift
    docker run -t -v $(pwd):$(pwd) -w $(pwd) -e USERID=$UID $container $@
};
