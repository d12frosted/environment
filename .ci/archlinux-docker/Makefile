DOCKER_USER:=d12frosted
DOCKER_ORGANIZATION=d12frosted
DOCKER_IMAGE:=environment
DOCKER_IMAGE_TAG:=archlinux

image:
	docker build --no-cache -t "$(DOCKER_ORGANIZATION)/$(DOCKER_IMAGE):$(DOCKER_IMAGE_TAG)" .

push:
	docker login -u $(DOCKER_USER)
	docker push "$(DOCKER_ORGANIZATION)/$(DOCKER_IMAGE):$(DOCKER_IMAGE_TAG)"

.PHONY: image push
