FROM debian:stable-20220622
ARG	bazel_version=5.1.1
ARG	bazel_url=https://storage.googleapis.com/bazel-apt/pool/jdk1.8/b/bazel/bazel_${bazel_version}_amd64.deb
RUN	apt-get update && apt-get install -y \
	    wget \
	&& wget "${bazel_url}" -nv -o- -O bazel.deb \
	&& apt-get -o Acquire::Retries=5 -o Acquire::http::Dl-Limit=800 install -y \
            ./bazel.deb \
            bash-completion \
            build-essential \
            default-jdk-headless \
            git \
            libfl2 \
            python-dev \
            python2.7-dev \
            python3-dev \
            vim \
            gcc \
            libffi-dev \
            libgmp-dev \
            libtinfo5 \
            libtinfo-dev \
            make \
            graphviz \
            sudo \
	&& rm bazel.deb \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/* \
	&& echo ". /etc/bash_completion" >> /root/.bashrc
ENV	NB_USER	jovyan
ENV	NB_UID	1000
ENV	HOME	/home/${NB_USER}
ENV     SHELL   /bin/bash
SHELL   ["/bin/bash", "-c"]
RUN	useradd --create-home --password asdf --uid ${NB_UID} ${NB_USER}
USER	root
RUN	chown -R ${NB_UID} ${HOME}
USER	${NB_USER}
WORKDIR	${HOME}
COPY workshop ${HOME}/workshop
RUN cd ${HOME}/workshop/01_simple && bazel build //:base
RUN \
  cd ${HOME}/workshop/02_haskell && \
  bazel build $(bazel query 'filter(stackage, kind(haskell_.*_library, deps(//:app)))' 2>/dev/null | tr '\n' ' ')
RUN \
  cd ${HOME}/workshop/03_haskell-gazelle && \
  bazel build $(bazel query 'filter(stackage, kind(haskell_.*_library, deps(//:app)))' 2>/dev/null | tr '\n' ' ') && \
  bazel build //:gazelle //:gazelle_mods
RUN \
  cd ${HOME}/workshop/04_polyglot && \
  bazel build $(bazel query 'filter(stackage, kind(haskell_.*_library, deps(//haskell:swagger)))' 2>/dev/null | tr '\n' ' ') && \
  bazel build $(bazel query 'filter(stackage, kind(haskell_.*_library, deps(//haskell:client)))' 2>/dev/null | tr '\n' ' ')
USER root
RUN rm -rf ${HOME}/workshop
USER	${NB_USER}
