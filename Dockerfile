## Multi-stage Dockerfile for Spring Boot Backend
FROM maven:3.9.9-eclipse-temurin-17 AS build
WORKDIR /app

# Leverage layer caching
COPY pom.xml .
RUN mvn -q -DskipTests dependency:go-offline

# Copy sources and build
COPY src ./src
RUN mvn -q -DskipTests package && \
    cp target/*.jar app.jar

FROM eclipse-temurin:17-jre-alpine AS runtime
WORKDIR /app

ENV JAVA_OPTS=""
ENV TZ=UTC

RUN addgroup -S spring && adduser -S spring -G spring
USER spring:spring

COPY --from=build /app/app.jar ./app.jar

EXPOSE 8080

ENTRYPOINT ["sh","-c","java $JAVA_OPTS -jar /app/app.jar"]
