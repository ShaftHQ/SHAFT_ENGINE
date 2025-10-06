# SHAFT Project Generator - Deployment Guide

## Table of Contents
1. [Deployment Options](#deployment-options)
2. [Local Deployment](#local-deployment)
3. [Cloud Deployment](#cloud-deployment)
4. [Docker Deployment](#docker-deployment)
5. [Production Considerations](#production-considerations)

## Deployment Options

The SHAFT Project Generator can be deployed in several ways:

1. **Local Deployment**: Run on your local machine or internal server
2. **Cloud Deployment**: Deploy to cloud platforms (Heroku, AWS, Azure, GCP)
3. **Docker Deployment**: Containerized deployment for consistency
4. **Static Hosting**: Deploy as a static website with backend API

## Local Deployment

### Prerequisites
- Java 21 or higher
- Maven 3.9.x or higher
- Sufficient disk space for generated projects

### Steps

1. **Build the application**:
   ```bash
   cd project-generator
   mvn clean package
   ```

2. **Run the JAR file**:
   ```bash
   java -jar target/shaft-project-generator-1.0.0.jar
   ```

3. **Access the application**:
   - Open browser: `http://localhost:8080`

### Running on a different port:
```bash
java -jar target/shaft-project-generator-1.0.0.jar --server.port=9090
```

### Running as a background service (Linux):
```bash
nohup java -jar target/shaft-project-generator-1.0.0.jar &
```

## Cloud Deployment

### Heroku Deployment

1. **Create a Heroku account** at https://www.heroku.com/

2. **Install Heroku CLI**:
   ```bash
   # macOS
   brew tap heroku/brew && brew install heroku
   
   # Ubuntu
   curl https://cli-assets.heroku.com/install.sh | sh
   ```

3. **Login to Heroku**:
   ```bash
   heroku login
   ```

4. **Create a new Heroku app**:
   ```bash
   cd project-generator
   heroku create shaft-project-generator
   ```

5. **Create a Procfile**:
   ```bash
   echo "web: java -jar target/shaft-project-generator-1.0.0.jar --server.port=\$PORT" > Procfile
   ```

6. **Deploy**:
   ```bash
   git init
   git add .
   git commit -m "Initial commit"
   git push heroku main
   ```

7. **Open the app**:
   ```bash
   heroku open
   ```

### AWS Elastic Beanstalk Deployment

1. **Install AWS CLI and EB CLI**:
   ```bash
   pip install awscli
   pip install awsebcli
   ```

2. **Initialize EB**:
   ```bash
   cd project-generator
   eb init -p java-21 shaft-project-generator
   ```

3. **Create environment**:
   ```bash
   eb create shaft-generator-env
   ```

4. **Deploy**:
   ```bash
   mvn clean package
   eb deploy
   ```

5. **Open the app**:
   ```bash
   eb open
   ```

### Azure App Service Deployment

1. **Install Azure CLI**:
   ```bash
   curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash
   ```

2. **Login to Azure**:
   ```bash
   az login
   ```

3. **Create resource group**:
   ```bash
   az group create --name ShaftGeneratorRG --location eastus
   ```

4. **Create App Service plan**:
   ```bash
   az appservice plan create --name ShaftGeneratorPlan --resource-group ShaftGeneratorRG --sku B1 --is-linux
   ```

5. **Create web app**:
   ```bash
   az webapp create --resource-group ShaftGeneratorRG --plan ShaftGeneratorPlan --name shaft-generator --runtime "JAVA|21-java21"
   ```

6. **Deploy**:
   ```bash
   mvn clean package
   az webapp deploy --resource-group ShaftGeneratorRG --name shaft-generator --src-path target/shaft-project-generator-1.0.0.jar --type jar
   ```

## Docker Deployment

### Create Dockerfile

Create a file named `Dockerfile` in the project-generator directory:

```dockerfile
FROM eclipse-temurin:21-jdk-alpine

# Set working directory
WORKDIR /app

# Copy Maven files
COPY pom.xml .
COPY src ./src
COPY templates ./templates

# Copy Maven wrapper
COPY .mvn ./.mvn
COPY mvnw .

# Build the application
RUN ./mvnw clean package -DskipTests

# Expose port
EXPOSE 8080

# Run the application
ENTRYPOINT ["java", "-jar", "target/shaft-project-generator-1.0.0.jar"]
```

### Build Docker Image

```bash
docker build -t shaft-project-generator:1.0.0 .
```

### Run Docker Container

```bash
docker run -p 8080:8080 shaft-project-generator:1.0.0
```

### Docker Compose

Create `docker-compose.yml`:

```yaml
version: '3.8'

services:
  shaft-generator:
    build: .
    ports:
      - "8080:8080"
    environment:
      - SERVER_PORT=8080
    restart: unless-stopped
```

Run with:
```bash
docker-compose up -d
```

### Push to Docker Hub

```bash
# Tag the image
docker tag shaft-project-generator:1.0.0 yourusername/shaft-project-generator:1.0.0

# Login to Docker Hub
docker login

# Push the image
docker push yourusername/shaft-project-generator:1.0.0
```

## Production Considerations

### Security

1. **HTTPS/SSL**:
   - Use reverse proxy (nginx, Apache) with SSL certificates
   - Consider Let's Encrypt for free SSL certificates

2. **CORS Configuration**:
   - Configure CORS if frontend is hosted separately
   - Add to `application.properties`:
     ```properties
     spring.web.cors.allowed-origins=https://yourdomain.com
     spring.web.cors.allowed-methods=GET,POST
     ```

3. **Authentication** (Optional):
   - Add Spring Security for authentication
   - Implement API keys or OAuth2

### Performance

1. **Resource Limits**:
   - Set JVM memory limits:
     ```bash
     java -Xmx512m -Xms256m -jar shaft-project-generator-1.0.0.jar
     ```

2. **Caching**:
   - Enable Spring caching for templates
   - Use CDN for static resources

3. **Load Balancing**:
   - Use multiple instances behind a load balancer
   - Consider Kubernetes for orchestration

### Monitoring

1. **Application Monitoring**:
   - Add Spring Boot Actuator
   - Integrate with monitoring tools (Prometheus, Grafana)

2. **Logging**:
   - Configure log levels in `application.properties`:
     ```properties
     logging.level.root=INFO
     logging.file.name=shaft-generator.log
     ```

3. **Health Checks**:
   - Add health check endpoint
   - Monitor application availability

### Backup and Recovery

1. **Template Backup**:
   - Regularly backup template files
   - Version control for templates

2. **Database** (if added):
   - Regular database backups
   - Point-in-time recovery

### Scalability

1. **Horizontal Scaling**:
   - Deploy multiple instances
   - Use session management (Redis, database)

2. **Vertical Scaling**:
   - Increase server resources (CPU, RAM)
   - Optimize Java heap size

### Update Strategy

1. **Rolling Updates**:
   - Deploy new version gradually
   - Keep old version running during deployment

2. **Blue-Green Deployment**:
   - Maintain two identical environments
   - Switch traffic after successful deployment

3. **Canary Deployment**:
   - Route small percentage of traffic to new version
   - Monitor and gradually increase traffic

## Maintenance

### Regular Tasks

1. **Update Dependencies**:
   ```bash
   mvn versions:display-dependency-updates
   mvn versions:use-latest-versions
   ```

2. **Security Patches**:
   - Monitor security advisories
   - Apply patches promptly

3. **Performance Tuning**:
   - Monitor application metrics
   - Optimize bottlenecks

4. **Template Updates**:
   - Keep templates up-to-date with latest SHAFT Engine version
   - Test template changes thoroughly

## Troubleshooting Deployment

### Issue: Port already in use
**Solution**:
```bash
# Find process using port 8080
lsof -i :8080

# Kill the process
kill -9 <PID>
```

### Issue: Out of memory
**Solution**:
```bash
# Increase heap size
java -Xmx1024m -jar shaft-project-generator-1.0.0.jar
```

### Issue: Application crashes on startup
**Solution**:
- Check logs: `tail -f logs/spring.log`
- Verify Java version: `java -version`
- Ensure templates directory exists
- Check file permissions

### Issue: Docker container exits immediately
**Solution**:
```bash
# Check logs
docker logs <container-id>

# Run in interactive mode
docker run -it shaft-project-generator:1.0.0 /bin/sh
```

## Support

For deployment issues or questions:
- Create an issue on GitHub
- Check SHAFT Engine documentation
- Contact SHAFT Engine support team

## Conclusion

This guide covers various deployment options for the SHAFT Project Generator. Choose the deployment method that best fits your organization's infrastructure and requirements.

For production deployments, prioritize security, monitoring, and scalability considerations.
