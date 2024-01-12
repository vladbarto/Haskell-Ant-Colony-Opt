import numpy as np
from scipy.spatial.distance import cdist
import numpy as np


def read_tsp_file(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()

    best_known_solution_index = lines.index("BEST_KNOWN_SOLUTION\n") + 1
    best_known_solution = lines[best_known_solution_index]

    coord_section_index = lines.index("NODE_COORD_SECTION\n") + 1
    data_lines = lines[coord_section_index:]

    coordinates = [list(map(int, line.split()[1:])) for line in data_lines if line.strip()]

    return np.array(coordinates), best_known_solution


# Read TSP instance from the provided .tsp file
file_path = 'TSPInstances/pla33810.tsp'  # Replace with the actual file path
coordinates, best_known_solution = read_tsp_file(file_path)

# Calculate the distance matrix from the coordinates
distance_matrix = np.zeros((len(coordinates), len(coordinates)))
for i in range(len(coordinates)):
    for j in range(len(coordinates)):
        distance_matrix[i, j] = np.linalg.norm(coordinates[i] - coordinates[j])


def ant_colony_optimization_tsp(distance_matrix):
    """
    This function implements the Ant Colony Optimization algorithm for the Traveling Salesman Problem.

    Parameters:
    distance_matrix (np.ndarray): The distance matrix for the TSP instance.

    Returns:
    list: The best tour found by the algorithm.
    float: The length of the best tour.
    """
    try:
        num_ants = 10
        num_iterations = 100
        num_cities = distance_matrix.shape[0]
        pheromone = np.ones((num_cities, num_cities))
        alpha = 1
        beta = 2
        rho = 0.5  # Evaporation rate

        def distance(x, y):
            return cdist(x.reshape(1, -1), y.reshape(1, -1), 'euclidean')[0][0]

        def calculate_tour_length(tour):
            length = 0
            for i in range(num_cities - 1):
                length += distance_matrix[tour[i], tour[i + 1]]
            length += distance_matrix[tour[-1], tour[0]]  # Return to the starting city
            return length

        for i in range(num_iterations):
            ant_tours = np.zeros((num_ants, num_cities), dtype=int)

            for k in range(num_ants):
                # Initialize ant positions
                current_city = np.random.randint(num_cities)
                visited_cities = [current_city]

                for _ in range(num_cities - 1):
                    # Calculate probabilities for the next city
                    probs = np.zeros(num_cities)
                    for l in range(num_cities):
                        if l not in visited_cities:
                            probs[l] = pheromone[current_city, l] ** alpha * (1 / distance_matrix[current_city, l] ** beta)
                    probs /= np.sum(probs)

                    # Choose the next city for the ant
                    next_city = np.random.choice(range(num_cities), p=probs)
                    visited_cities.append(next_city)
                    current_city = next_city

                ant_tours[k, :] = visited_cities

            # Update pheromone levels
            for k in range(num_ants):
                tour_length = calculate_tour_length(ant_tours[k, :])
                for l in range(num_cities - 1):
                    pheromone[ant_tours[k, l], ant_tours[k, l + 1]] += 1 / tour_length
                pheromone[ant_tours[k, -1], ant_tours[k, 0]] += 1 / tour_length  # Return to the starting city

            # Evaporate pheromone
            pheromone *= (1 - rho)

        # Find the best tour
        best_tour_index = np.argmin([calculate_tour_length(tour) for tour in ant_tours])
        best_tour = ant_tours[best_tour_index, :]
        best_tour_length = calculate_tour_length(best_tour)

        return best_tour, best_tour_length

    except Exception as e:
        # Log the error
        print(f"Error: {e}")
        return None, None

best_tour, best_tour_length = ant_colony_optimization_tsp(distance_matrix)
print("Best Tour:", best_tour)
print("Best Tour Length:", best_tour_length)
print(f"Best Known Solution: {best_known_solution}")