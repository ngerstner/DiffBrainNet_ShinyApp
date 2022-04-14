## Documentation: Dockerizing the app

- start Docker engine on the computer
- check existing images with `docker images` -> the image, your image is based on, needs to be there
- from project folder (one folder above diffbrainnet folder) where the *Dockerfile* needs to be stored: `docker build -t ngerst/diffbrainnet:v6 .` (or the version one wants to build)
- run container to try it out with `docker run -d --rm -p 3838:3838 ngerst/diffbrainnet:v6``
- in your browser open *localhost:3838* to see the app
- run `docker tag ngerst/diffbrainnet:v6 ngerst/diffbrainnet:latest` to set the latest tag to the current image
- run `docker push ngerst/diffbrainnet:v6` and 'docker push ngerst/diffbrainnet:latest` to publish image on *Docker Hub*