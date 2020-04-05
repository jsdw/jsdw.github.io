+++
title = "Notes on Postgres"
description = "Various snippets of useful information and examples for PostgreSQL"
date = 2020-04-04
draft = false
[extra]
toc = 1
+++

You can use `psql` to connect to a Postgres database. For example, to connect to the `postgres` database on `localhost`:

```
psql -U postgres -h localhost -d postgres
```

Use `\?` for a list of useful commands. A couple of the useful ones are:

- `\l` to list all databases.
- `\c database_name` to connect to a new database.
- `\d schema_name.table_name` to describe a table (schema_name can be omitted, either can be '*' to match all).
- `\dn+` to display the schemas in the current database with privileges (no '+' for no privileges).
- `\du` to display the roles and memberships.
- `\dt schema_name.*` to list tables present in some schema.

A brief explanation of a couple of the moving parts:

**Roles**: Roles can be assigned privileges. Users are just roles that are able to login and have passwords. Roles can be members of other roles. Roles can allow their privileges to be inherited by their members or require that members explicitly `SET ROLE` to use their privileges.

**Database**: A Postgres instance on your machine can have multiple *databases*. Each database has its own types, tables, views and so on, but roles are shared across all databases. A connection is tied to a specific database.

**Schema**: A database can contain numerous *schemas*. By default a `public` schema exists, which new tables and such are created in if not otherwise specified. Each schema can have its own tables and such. Your current *search path* determines what schema you'll create things in and get things from if you don't specify one.

**Tables**: Tables contain the actual data. They always live within a schema.

The built-in schema `pg_catalog` contains a bunch of tables that themselves contain various metadata about the system, such as the users (`pg_catalog.pg_roles`), databases (`pg_catalog.pg_database`), various statistics like active connections (`pg_catalog.pg_stat_activity`) and much more. Some of these are specific to that database you're connected to and some (like `pg_roles`) are shared across all databases.

# Users and Roles

In Postgres, users are just roles that you can log into the database with. Roles are shared across all Postgres databases.

Working with roles looks a bit like:

```
-- Make a new user:
CREATE ROLE a WITH LOGIN PASSWORD 'somepass';

-- Make a new role (can't login as this):
CREATE ROLE b;

-- Grant privileges to 'b'
GRANT SELECT, INSERT ON SCHEMA public TO b;
GRANT ALL ON TABLE some_table TO b;

-- Make 'a' a member of 'b' so it inherits privileges:
GRANT b TO a;

-- Remove 'b' from 'a' so it no longer inherits privileges:
REVOKE b FROM a;
```

See [here][sql-createrole] for more on roles.
See [here][sql-grant] for granting privileges.

Various things alter the privileges that a role ultimately has:

- Roles can be `GRANT`ed privileges in order to increase what they are allowed to do.
- Roles can be members of other roles to acquire the privileges granted on them.
- Roles can have attributes like `CREATEDB` and `CREATEROLE` which grant them extra abilities above what `GRANT`ed privileges allow.
- Tables and schemas (and other objects) are owned by a role.
    - Owners can also grant themselves whatever privileges they desire on things that they own.
    - Only the owner of a table or schema or a superuser can `DROP` or `ALTER` it.
- A `PUBLIC` pseudo-role exists that every role is a member of.
    - It grants a very limited set of general permissions to roles.
    - It grants all permissions on the `public` schema.
    - You can revoke these permissions.
- `DEFAULT PRIVILEGES` exist and can be altered such that roles gain privileges automatically on newly created things.
    - These can be granted only when objects are created in a given schema (`IN SCHEMA`).
    - These can be granted only when objects are created by a specific role (`FOR ROLE`).

It can be quite cumbersome discovering what privileges a given role has.

See [the docs][ddl-priv] for more info on privileges, what they each mean, and how to examine them.

## Examining privileges

Privileges are displayed in the form `grantee=arwdDxt/grantor`, where `grantee` is the user being granted the privileges (if empty, everybody gets the privileges), `arwdDxt` is a set of letters each denoting a particular permisison (see [the docs][ddl-priv] for info on this), and `grantor` is the user who granted those privileges.

In `psql`, we can inspect privileges using some of the following (in the database you're connected to, at least):

- `\du` to see what roles are members of what.
- `\l` shows databases and privileges of roles on those.
- `\dn+` shows schemas and schema access privileges.
- `\dp schemaname.tablename` to find access privileges on some table (+ `\dt` to see the table owner).
- `\dpp schemaname.tablename` to view the default privileges on some table.

Note that these do not resolve role memberships. Postgres also has a number of [access functions][access-functions] that you can run to enquire about privieleges. A couple of usage examples:

```
-- What tables does 'username' have INSERT on?
SELECT oid, relname FROM pg_class WHERE has_table_privilege('username', oid, 'INSERT');
-- Does 'username' have 'CREATE' permission on 'dbname'?
SELECT has_database_privilege('username', 'dbname', 'CREATE');
```

These to resolve role memberships (but only those inherited) to work out the privileges.

## Default and PUBLIC privileges

When you create roles, they inherit privileges from roles that they are a member of, but also from the `PUBLIC` pseudo-role, which every role inherits privileges from. This `PUBLIC` pseudo-role grants all privileges on the `public` schema by default, and limited permissions to the database in general and a couple of other things.

- You can revoke permissions on PUBLIC. Running `REVOKE ALL ON SCHEMA public FROM PUBLIC` prevents roles from being able to act in the `public` schema, for instance.
- You can also grant privileges to `PUBLIC` to give them to every role in the system.

Roles are also granted privileges according to any *default privileges* that exist.

Read [the docs][sql-alterdefaultprivileges] for more on default privileges.

## Dropping roles

Dropping roles can be achieved by `DROP ROLE rolename`, though if they own anything in any database, that will not work. One way to get around this is `DROP OWNED BY rolename`. This drops everythng owned by the specified role, revoking any grants along the way.

# Databases

Create and drop a database with:

```
CREATE DATABASE dbname OWNER username;
DROP DATABASE dbname;
```

- Connections are tied to a specific database, so if you have multiple databases, you'll need to establish multiple connections to talk to them.
- Roles are shared across the databases in a *cluster* (running instance of Postgres). Most other things are database specific.
- A database can contain multiple *schemas*, which are a bit like namespaces or "folders" and help to keep things separate.
- You cannot write SQL queries that span more than one database (eg `JOIN`s), but they can span multiple schemas.

# Schemas

Create and drop a schema (`AUTHORIZATION` sets the owner of a schema):

```
CREATE SCHEMA foo AUTHORIZATION username;
DROP SCHEMA foo;
```

The owner of a schema is like a mini-superuser within that schema. You can lock things down better by having a different role owning the schema to that which accesses it.

A *search path* is associated with either the database (which applies to all roles unless overridden for a role) or specific role.
- If we perform a query without including a specific schema name, the search path determines what schema will be searched and in what order to find the object you're trying to use.
- A special `$user` variable in the search path is equal to whatever the current role name is.
- `pg_catalog` is by default the first schema that will be searched regardless of the search path set. Naming it explicitlty in the search path can change when it is searched.

```
-- Show the current user's search path:
SHOW search_path;
-- Set the search path for the current user for this session:
SET search_path = foo, bar;
-- Permanently alter the search path for any role in a database:
ALTER DATABASE dbname SET search_path = foo, bar;
-- Alter the search path for a given role:
ALTER ROLE rolename SET search_path = foo, bar;
```

Aside from looking up things that arent expicitly qualified by a schema name, the first valid schema in the search path is where new items will be created. The `public` schema is included in the search path by default, so new tables and such that are not explicitly qualified by a schema will appear there without further configuration.



[sql-createrole]: https://www.postgresql.org/docs/12/sql-createrole.html
[sql-grant]: https://www.postgresql.org/docs/12/sql-grant.html
[ddl-priv]: https://www.postgresql.org/docs/12/ddl-priv.html
[sql-alterdefaultprivileges]: https://www.postgresql.org/docs/12/sql-alterdefaultprivileges.html
[access-functions]: https://www.postgresql.org/docs/12/functions-info.html#FUNCTIONS-INFO-ACCESS-TABLE