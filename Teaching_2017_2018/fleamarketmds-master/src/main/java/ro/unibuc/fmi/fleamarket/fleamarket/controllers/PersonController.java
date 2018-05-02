package ro.unibuc.fmi.fleamarket.fleamarket.controllers;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import ro.unibuc.fmi.fleamarket.fleamarket.domain.Person;
import ro.unibuc.fmi.fleamarket.fleamarket.repository.PersonRepository;

import javax.validation.Valid;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/persons")
public class PersonController {

    @Autowired
    private PersonRepository personRepository;

    @RequestMapping(method = RequestMethod.GET, produces = "application/json")
    public List<Person> persons() {
        return personRepository.findAll();
    }

    @RequestMapping(method = RequestMethod.GET, value = "/{id}")
    public Optional<Person> getPersonById(@PathVariable("id") Long id) {
        return personRepository.findById(id);
    }

    @CrossOrigin
    @RequestMapping(method = RequestMethod.POST)
    public void savePerson(@RequestBody @Valid Person person) {
        person.setId(null);
        personRepository.save(person);
    }

    @RequestMapping(method = RequestMethod.PUT, value = "/{id}")
    public void editPerson(@RequestBody @Valid Person editPerson, @PathVariable("id") Long id) {
        editPerson.setId(id);
        personRepository.saveAndFlush(editPerson);
    }
}
