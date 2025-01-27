/* eslint-disable */
// this is an auto generated file. This will be overwritten

export const getWorkout = /* GraphQL */ `
  query GetWorkout($id: ID!) {
    getWorkout(id: $id) {
      id
      date
      exercise
      sets
      reps
      weight
      createdAt
      updatedAt
      __typename
    }
  }
`;
export const listWorkouts = /* GraphQL */ `
  query ListWorkouts(
    $filter: ModelWorkoutFilterInput
    $limit: Int
    $nextToken: String
  ) {
    listWorkouts(filter: $filter, limit: $limit, nextToken: $nextToken) {
      items {
        id
        date
        exercise
        sets
        reps
        weight
        createdAt
        updatedAt
        __typename
      }
      nextToken
      __typename
    }
  }
`;
