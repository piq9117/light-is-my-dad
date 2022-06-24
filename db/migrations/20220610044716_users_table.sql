-- migrate:up
create table users (
  Id UUID NOT NULL PRIMARY KEY DEFAULT uuid_generate_v4(),
  name TEXT NOT NULL,
  age SMALLINT NOT NULL,
  email TEXT NOT NULL UNIQUE,
  password TEXT NOT NULL,
  registration_date DATE NOT NULL
);

-- migrate:down

