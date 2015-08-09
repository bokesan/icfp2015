package main;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import solver.PathFinder;

public class Statistics {
	
	private List<Solution> solutions = new ArrayList<>();
	int problems, seeds, total_points, used_modes = 0;
	ProblemStats[] problemstats;

	public void add(List<Solution> solutions) {
		this.solutions.addAll(solutions);
	}

	public void writeStatsFile(String string) {
		processSolutions();
		
	}

	private void processSolutions() {
		Set<Integer> problems = new TreeSet<Integer>();
		List<PathFinder.Mode> modes = new ArrayList<>();
		for (Solution solution : solutions) {
			problems.add(solution.id);
			seeds++;
			total_points += solution.points;
		}
		
	}
	
	private class ProblemStats {
		
	}

}
