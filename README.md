# Backend Service (Spring Boot)

Backend service for the [Database Management System](https://github.com/ilias363/db-manager-app). Provides authentication, authorization, persistence, audit logging & API surface consumed by the [Next.js frontend](https://github.com/ilias363/db-management-fe).

## Tech
Spring Boot 3, JPA, MySQL, JWT (jjwt), Spring Security, Validation, Lombok.

## Quick Run (Local)
```
./mvnw spring-boot:run
```
Ensure MySQL reachable (adjust `application.properties`).

## Docker
Runs as `backend` service in the root [`docker-compose.yml`](https://github.com/ilias363/db-manager-app/blob/master/docker-compose.yml) (internal port 8080).
