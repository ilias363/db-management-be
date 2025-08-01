package ma.ilias.dbmanagementbe;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
public class DbManagementBeApplication {

    public static void main(String[] args) {
        SpringApplication.run(DbManagementBeApplication.class, args);
    }
}
