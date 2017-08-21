# Aliases for various jupyter notebook-related things which I find
# helpful to keep handy, like being able to connect to a notebook
# running on a remote machine, for example.

function connect_notebook_ssh {
    # Connect to a notebook running on another machine via an ssh
    # port-forward
    if [ -z "$2" ]
    then
	local=8888
    else
	local=$2
    fi
    if [ -z "$3" ]
       then
	   remote=8888
    else
	remote=$3
    fi    
    
    ssh -N -f -L localhost:$local:localhost:$remote $1
    sensible-browser localhost:$local &
};

function headless_notebook {
    # Start a notebook in a headless state
    if [ -z "$1" ]
    then
	port=8888
    else
	port=$1
    fi
    jupyter notebook --no-browser --port $port &
}

function jupyter_add_venv {
    # Add a virtual environment to jupyter's list of available kernels
    workon $1
    python -m ipykernel install --user --name venv-$1 --display-name "Python ($1)"
}

function jupyter_remove_venv {
    yes "y" | jupyter kernelspec uninstall venv-$1
}
