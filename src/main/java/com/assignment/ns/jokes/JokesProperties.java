package com.assignment.ns.jokes;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Data
@Component
@ConfigurationProperties("jokes")
public class JokesProperties {

    private String uri;
}
