```
yarn start
```

in another terminal

```
yarn start:proxy
```

```
pscid
```
to watch for changes

## To Add A Dependency

```
psc-package install <package>
psc-package build
```

if it's prefixed with purescript, you leave that bit off so to install `purescript-routing` it's `psc-package install routing`

## TODO

### Authentication
- Figure out why cookie isn't being preserved after refresh

### Users Screen
- Parse JSON data on users screen
- Render users as a list
- Make users followable
