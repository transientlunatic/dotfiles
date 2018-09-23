function dockerenv {
    docker run -it -v $(pwd):$(pwd) -w $(pwd) -e USERID=$UID $1 /bin/bash
};
