import * as inflection from 'inflection';

export const inflect = string => count => {
  return inflection.inflect(string, count);
}
