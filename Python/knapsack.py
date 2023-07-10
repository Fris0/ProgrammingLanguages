import random
from copy import deepcopy


class knapsack():
    def __init__(self, result):
        self.result = result

    def save(self, solution_file):
        """
        Write the solution to a csv file.
        solution_file: The name of the file to be written to.

        Side-effect: Writing to external file.
        """
        f = open(solution_file, "a")
        f.write("points:" + str(self.result[0]) + "\n")
        for i in self.result[1:]:
            f.write(str(i) + "\n")

    def print(self):
        """
        Print the results to standard output.

        Side-effect: Print to STD Out.
        """
        print("points:" + str(self.result[0]))
        for i in self.result[1:]:
            print(i)


class Solver_Random:
    def __init__(self, reps):
        """
        Initialize the class variables.

        Reps: Int that holds amount of reps.
        """
        self.reps = reps
        self.seed = random.seed()  # For distinct random vals with disti reps.
        self.result = [0]

    def solve(self, knapsack, items):
        """
        Solve the double constraint knapsack
        problem by guessing multiple time.

        Knapsack: Dict holding the sack info.
        Items: Dict holding the items and values.
        """
        chosen = set()  # Store random chosen items. Prevents unneeded copies.
        keys = list(items.keys())

        for i in range(self.reps):
            space = True
            points = knapsack['knapsack'][0]
            weight = knapsack['knapsack'][1]
            volume = knapsack['knapsack'][2]
            choices = [0]

            self.change_seed()  # Change the seed.

            while (space):
                key = random.choice(keys)
                if key not in chosen:
                    chosen.add(key)
                    item = items[key]
                    weight = weight - item[1]
                    volume = volume - item[2]
                    if weight < 0 or volume < 0:
                        space = False
                    else:
                        choices.append(key)
                        points = points + item[0]
                else:
                    continue

            choices[0] = points
            chosen = set()  # Empty the list of choices that were made.

            if choices[0] > self.result[0]:
                self.result = choices

    def __len__(self, keys):
        """
        Return the length of the keys.

        Output: Key length.
        """
        return (len(keys))

    def change_seed(self):
        """
        Change the seed.
        """
        self.seed = random.seed()

    def get_best_knapsack(self):
        """
        Return the best result.

        Output: List with points and choices.
        """
        sack = knapsack(self.result)
        return (sack)


class Solver_Random_Improved(Solver_Random):
    def __init__(self, reps):
        # Overwrite the init reps value of parent class.
        Solver_Random.__init__(self, reps)

    def solve(self, knapsack, items):
        """
        Optimized the Solver_Random class
        by using replacement.

        Knapsack: Dict holding the sack info.
        Items: Dict holding the items and values.
        """
        chosen = set()
        keys = list(items.keys())
        set_keys = set(keys)

        for i in range(self.reps):
            space = True
            points = knapsack['knapsack'][0]
            weight = knapsack['knapsack'][1]
            volume = knapsack['knapsack'][2]
            choices = [0]

            self.change_seed()  # Change the seed.

            while (space):
                key = random.choice(keys)
                if key not in chosen:
                    chosen.add(key)
                    item = items[key]
                    weight = weight - item[1]
                    volume = volume - item[2]
                    if weight < 0 or volume < 0:
                        space = False
                    else:
                        choices.append(key)
                        points = points + item[0]
                else:
                    continue

                if (not space):
                    distinct = set_keys.difference(chosen)
                    for i in distinct:
                        random_key = random.choice(choices)
                        try:
                            bool1 = (items[i][1] < items[random_key][1])
                            bool2 = (items[i][2] < items[random_key][2])
                            if (bool1 and bool2):
                                if (items[i][0] > items[random_key][0]):
                                    weight = (weight + items[random_key][1]
                                              + items[i][1])
                                    volume = (volume + items[random_key][2]
                                              + items[i][2])
                                    points = (points - items[random_key][0]
                                              + items[i][0])
                                    choices.remove(random_key)
                                    choices.append(i)
                        except KeyError:
                            continue

            choices[0] = points
            chosen = set()  # Empty the list of choices that were made.

            if choices[0] > self.result[0]:
                self.result = choices


class Solver_Optimal_Recursive:
    def __init__(self):
        self.result = [0]
        self.choices = None

    def solve(self, knapsack, items):
        """
        Calls the solve_for function
        because it requires an extra variable
        compared to the other solvers.

        Knapsack: Dict holding the sack info.
        Items: Dict holding the items and values.
        """
        self.solve_for(knapsack, items, [])

    def solve_for(self, knapsack, items, choices):
        """
        Recursive solution to the optimal double constraint
        knapsack problem.

        Knapsack: Dict holding the sack info.
        Items: Dict holding the items and values.
        Choices: Holds order of choices of prev function calls.
        """
        if knapsack['knapsack'][0] > self.result[0]:
            self.result[0] = knapsack['knapsack'][0]
            self.choices = choices
        if (items):

            cpy = items.copy()

            l_sack = knapsack.copy()
            r_sack = knapsack

            l_choices = choices.copy()
            r_choices = choices

            item = list(cpy.keys())[0]

            values = items[item]
            points = knapsack['knapsack']

            bool1 = knapsack['knapsack'][1] - values[1] >= 0
            bool2 = knapsack['knapsack'][2] - values[2] >= 0

            if bool1 and bool2:
                l_choices.append(item)
                l_sack['knapsack'] = (points[0] + values[0],
                                      points[1] - values[1],
                                      points[2] - values[2])

                cpy.pop(item)

                self.solve_for(l_sack, cpy, l_choices)
                self.solve_for(r_sack, cpy, r_choices)
            else:
                cpy.pop(item)
                self.solve_for(r_sack, cpy, r_choices)

    def get_best_knapsack(self):
        sack = knapsack(self.result + self.choices)
        return (sack)


def load_knapsack(knapsack_file):
    """
    Read lines from *.csv file and store the
    values in the (dict) variable they belong to.

    knapsack_file: The csv file to be read.
    """
    file = open(knapsack_file, "r")
    file.readline()  # Skip the first line of the file.

    knapsack = dict()
    items = dict()

    for line in file:
        line = line.split(", ")
        line[-1] = line[-1].split()[0]  # Remove newline from last elem.
        val = [int(i) for i in line[1:]]  # Change values from string to int.

        if (line[0] != 'knapsack'):
            items[line[0]] = (val[0], val[1], val[2])
        else:
            knapsack[line[0]] = (val[0], val[1], val[2])

    return knapsack, items


class Solver_Optimal_Iterative:
    def __init__(self):
        self.result = [0]
        self.optimal_choice = None

    def solve(self, knapsack, items):
        """
        Solve Knapsack problem
        using iteration and no deepcopy.

        Knapsack: Dict holding the sack info.
        Items: Dict holding the items and values.
        """
        queue = []
        keys = list(items.keys())
        values = knapsack['knapsack']

        for key in keys:
            extend = []
            if bool(queue):
                for _list in queue:
                    extend.append(_list + [key])
            else:
                extend.append([key])
                extend.append([])
            queue = queue + extend

        # Iterative over alle mogelijke combinaties tot eind resultaat.
        for comb in queue:
            points = sum((items[i][0] for i in comb))
            weight = sum((items[i][1] for i in comb))
            volume = sum((items[i][2] for i in comb))

            bool1 = points > self.result[0]
            bool2 = values[1] - weight >= 0
            bool3 = values[2] - volume >= 0
            if (bool1 and bool2 and bool3):
                self.result[0] = points
                self.optimal_choice = comb

    def get_best_knapsack(self):
        """
        Return the best possible result.
        """
        sack = knapsack(self.result + self.optimal_choice)
        return (sack)


class Solver_Optimal_Iterative_Deepcopy(Solver_Optimal_Iterative):
    def solve(self, knapsack, items):
        """
        Solve Knapsack problem
        using iteration and deepcopy.

        Knapsack: Dict holding the sack info.
        Items: Dict holding the items and values.
        """
        queue = []
        keys = list(items.keys())
        values = knapsack['knapsack']

        for key in keys:
            if (bool(queue)):
                combs = deepcopy(queue)
                for comb in combs:
                    comb = comb + [key]
                    queue.append(comb)
            else:
                queue.append([])
                queue.append([key])

        # Iterative over alle mogelijke combinaties tot eind resultaat.
        for comb in queue:
            points = sum((items[i][0] for i in comb))
            weight = sum((items[i][1] for i in comb))
            volume = sum((items[i][2] for i in comb))

            bool1 = points > self.result[0]
            bool2 = values[1] - weight >= 0
            bool3 = values[2] - volume >= 0
            if (bool1 and bool2 and bool3):
                self.result[0] = points
                self.optimal_choice = comb


def main():
    """
    Calls different solver classes
    to solve the Knapsack problem.
    """
    solver_random = Solver_Random(1000)
    solver_optimal_recursive = Solver_Optimal_Recursive()
    solver_optimal_iterative_deepcopy = Solver_Optimal_Iterative_Deepcopy()
    solver_optimal_iterative = Solver_Optimal_Iterative()
    solver_random_improved = Solver_Random_Improved(5000)

    #knapsack_file = "knapsack_small"
    #print("=== solving:", knapsack_file)
    #solve(solver_random, knapsack_file+".csv",
    #      knapsack_file+"_solution_random.csv")
    #solve(solver_optimal_recursive, knapsack_file+".csv",
    #      knapsack_file+"_solution_optimal_recursive.csv")
    #solve(solver_optimal_iterative_deepcopy, knapsack_file+".csv",
    #      knapsack_file+"_solution_optimal_iterative_deepcopy.csv")
    #solve(solver_optimal_iterative, knapsack_file+".csv",
    #      knapsack_file+"_solution_optimal_iterative.csv")
    #solve(solver_random_improved, knapsack_file+".csv",
    #      knapsack_file+"_solution_random_improved.csv")

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file+".csv",
          knapsack_file+"_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file+".csv",
          knapsack_file+"_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file+".csv",
          knapsack_file+"_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file+".csv",
          knapsack_file+"_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file+".csv",
          knapsack_file+"_solution_random_improved.csv")
    #knapsack_file = "knapsack_large"
    #print("=== solving:", knapsack_file)
    #solve(solver_random, knapsack_file+".csv",
    #      knapsack_file+"_solution_random.csv")
    #solve(solver_random_improved, knapsack_file+".csv",
    #      knapsack_file+"_solution_random_improved.csv")
#

def solve(solver, knapsack_file, solution_file):
    """
    Uses 'solver' to solve the knapsack problem in file
    'knapsack_file' and writes the best solution to 'solution_file'.
    """
    knapsack, items = load_knapsack(knapsack_file)
    solver.solve(knapsack, items)
    knapsack = solver.get_best_knapsack()
    knapsack.print()
    knapsack.save(solution_file)


# keep these as last lines
if __name__ == "__main__":
    main()
