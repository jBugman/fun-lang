FROM jbugman/funlang-circleci:0.1

COPY . /home/circleci/fun-lang

USER root
RUN chown circleci:circleci -R /home/circleci/
USER circleci

WORKDIR /home/circleci/fun-lang

CMD ["bash"]
