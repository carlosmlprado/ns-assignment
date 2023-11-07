package com.assignment.ns.jokes;

import com.assignment.ns.jokes.dto.request.JokesRequest;
import org.springframework.http.MediaType;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange(accept = MediaType.APPLICATION_JSON_VALUE)
public interface JokesClient {

    @GetExchange
    JokesRequest getJokes();
}
