Run [Jupyter](http://jupyter.org/) notebook with
[IOCaml](https://github.com/andrewray/iocaml/). This container is useful to run
the notebooks included in this project.

## Build

    docker build --force-rm=true -t iocaml

## Run

    docker run -it -v /abspath/to/folder/with/notebooks:/root/notebooks -p 8888:8888 oh-my-ocaml
