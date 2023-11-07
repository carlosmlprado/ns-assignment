package com.assignment.ns.jokes.exceptions.handler;

import com.assignment.ns.jokes.exceptions.JokeBadRequestException;
import com.assignment.ns.jokes.exceptions.JokeInternalServerErrorException;
import com.assignment.ns.jokes.exceptions.JokeNotFoundException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

@ControllerAdvice
public class ControllerExceptionHandler extends ResponseEntityExceptionHandler {

    @ExceptionHandler(value = {JokeNotFoundException.class})
    protected ResponseEntity<Object> jokeNotFoundException(final RuntimeException ex, final WebRequest request) throws JokeNotFoundException {
        final String bodyOfResponse = "Joke Not Found!";
        return handleExceptionInternal(ex, bodyOfResponse, new HttpHeaders(), HttpStatus.NOT_FOUND, request);
    }

    @ExceptionHandler(value = {JokeInternalServerErrorException.class})
    protected ResponseEntity<Object> jokeInternalServerErrorException(final RuntimeException ex, final WebRequest request) throws JokeInternalServerErrorException {
        final String bodyOfResponse = "Unfortunately we couldn't retrieve your joke! Please try again later!";
        return handleExceptionInternal(ex, bodyOfResponse, new HttpHeaders(), HttpStatus.INTERNAL_SERVER_ERROR, request);
    }

    @ExceptionHandler(value = {JokeBadRequestException.class})
    protected ResponseEntity<Object> jokeBadRequestException(final RuntimeException ex, final WebRequest request) throws JokeInternalServerErrorException {
        final String bodyOfResponse = "Unfortunately we couldn't retrieve your joke! It's our fault! We are working on It! Please try again later!";
        return handleExceptionInternal(ex, bodyOfResponse, new HttpHeaders(), HttpStatus.BAD_REQUEST, request);
    }

}