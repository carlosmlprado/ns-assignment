package com.assignment.ns.jokes.exceptions;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.NOT_FOUND)
public class JokeNotFoundException extends RuntimeException {

    public JokeNotFoundException() {
        super();
    }
}
