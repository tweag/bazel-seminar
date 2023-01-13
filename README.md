# Bazel Workshop

## Usage

Follow these steps to build and start the seminar image.
It is assumed that the repository is checked out into `$REPO`.

1. Build the Docker image containing bazel and other deps:

    ```
    $ docker build . -t bazel_seminar
    ```

1. Start the docker image, mounting the workshop dir:

    ```
    $ docker run \
        -v$REPO/workshop:/home/jovyan/workshop \
        -it bazel_seminar:latest \
        bash

    ```

Since the `workshop` directory is mounted, you can use whatever you like to edit the definitions within.

## Slides

There is currently a [presentation](workshop/slides.md), which is intended to be used with [marp](https://marp.app/).

Despite this, it is mostly vanilla markdown, so it should be easily adaptable if needed.

## TODO

- [x] Write workshop material
- [x] Add caching (disk & repository) configuration
- [x] Create self-contained Docker image
  - [?] Pre-build all the Bazel workspaces to populate the execroot and a disk-cache
    - haven't prebuilt a disk cache, but execroot is populated
- [?] Host the image for the workshop
  - use gdoc for now?
  - Requirements
    - Every participant should get their own instance
  - Options
    - Participants run docker locally
    - Host on binder
    - Self hosted on cluster that spins up instances on demand
