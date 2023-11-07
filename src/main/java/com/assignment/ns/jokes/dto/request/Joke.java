package com.assignment.ns.jokes.dto.request;

public record Joke(String joke, Integer id, Flag flags, Boolean safe) {}
