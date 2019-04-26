DOCKER_USER:=d12frosted
DOCKER_ORGANIZATION=d12frosted
DOCKER_IMAGE:=environment
DOCKER_IMAGE_TAG:=archlinux

docker-image:
	docker build -t "$(DOCKER_ORGANIZATION)/$(DOCKER_IMAGE):$(DOCKER_IMAGE_TAG)" .

docker-push:
	docker login -u $(DOCKER_USER)
	docker push "$(DOCKER_ORGANIZATION)/$(DOCKER_IMAGE):$(DOCKER_IMAGE_TAG)"

.PHONY: rootfs docker-image docker-image-test ci-test docker-push
