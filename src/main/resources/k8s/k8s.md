# Playing with K8s on Windows

- upgrade all existing apps (optional)

```shell
winget upgrade --silent --force --all --include-unknown
```

- install helm

```shell
winget search helm
```

```shell
winget install Helm.Helm
```

- restart powershell (close all windows and reopen)
- install keda # https://keda.sh/docs/2.10/deploy/#install

```shell
helm repo add kedacore https://kedacore.github.io/charts
```

```shell
helm repo update
```

```shell
kubectl create namespace selenium-grid
```

```shell
helm install keda kedacore/keda --namespace selenium-grid
```

```shell
kubectl config set-context --current --namespace=selenium-grid
```

- install selenium-grid # https://github.com/SeleniumHQ/docker-selenium/tree/trunk/charts/selenium-grid

```shell
helm repo add docker-selenium https://www.selenium.dev/docker-selenium
```

```shell
helm repo update
```

```shell
# Install distributed grid latest version
helm install selenium-grid docker-selenium/selenium-grid --set isolateComponents=true
```

```shell
# Updating Selenium-Grid release
helm upgrade selenium-grid docker-selenium/selenium-grid
```

- make grid reachable
  - ensure pods are running (proceed after all are "Running", it will take some time if your machine is downloading new
    containers)
      ```shell
      kubectl get pods
      ```
  - get pod name
      ```shell
      #kubectl get pods --all-namespaces
      kubectl get pods --selector="app=selenium-router" --output=template --template="{{with index .items 0}}{{.metadata.name}}{{end}}"
      ```
  - port forward the pod, keep this command running (ex. selenium-hub-dcf9dfb5b-vcbzg OR
    selenium-router-5f6bcfb596-sslwl)
      ```shell
      kubectl port-forward selenium-router-5f6bcfb596-sslwl 4444:4444
      ```
    - validate that grid is reachable (Note: VNC password is "secret")
        ```shell
        curl http://localhost:4444
        ```
- apply auto-scaling
    ```shell
    kubectl apply -f https://github.com/kedacore/keda/releases/download/v2.10.0/keda-2.10.0.yaml
    ```
    ```shell
    kubectl apply -f ./selenium-grid-chrome-scaledobject.yaml
    ```
    ```shell
    kubectl apply -f ./selenium-grid-edge-scaledobject.yaml
    ```
    ```shell
    kubectl apply -f ./selenium-grid-firefox-scaledobject.yaml
    ```
- validate auto-scaling by confirming all three scaled objects have "READY=True"
    ```shell
    kubectl get scaledobjects --all-namespaces
    ```
- get logs from keda-operator node
    ```shell
    kubectl get pods -n keda
    ```
    ```shell
    kubectl logs -n keda keda-operator-68686748d8-6bthv -c keda-operator
    ```
- teardown (wait till each step is completed before proceeding to the next one)
    ```shell
    kubectl delete scaledobjects --all
    ```
    ```shell
    kubectl delete pods --all
    ```
    ```shell
    kubectl delete services --all
    ```
    ```shell
    kubectl delete all --all
    ```
    ```shell
    kubectl delete namespace selenium-grid
    ```
    ```shell
    helm del $(helm ls --all --short)
    ```

#https://www.selenium.dev/blog/2022/scaling-grid-with-keda/#from-start-to-finish