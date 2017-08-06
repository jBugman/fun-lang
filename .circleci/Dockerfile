FROM circleci/golang:1.8.3

ENV PATH="/home/circleci/.local/bin:${PATH}"

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN stack upgrade

RUN mkdir -p /home/circleci/.local/bin && \
    wget -qO- https://github.com/jBugman/hpc-coveralls/releases/download/stack/coveralls.tar.gz | \
    tar xvz -C /home/circleci/.local/bin

RUN go get -u github.com/golang/dep/cmd/dep
