package main;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import solver.PathFinder;

public class Statistics {
	
	private List<Solution> solutions = new ArrayList<>();
	int seeds, total_points = 0;
	ProblemStats[] problemstats;
	ModeStats[] modestats;

	public void add(List<Solution> solutions) {
		this.solutions.addAll(solutions);
	}

	public void writeStatsFile(String file, long duration) {
		processSolutions();
		
        PrintWriter writer;
        try {
            writer = new PrintWriter(file, "UTF-8");
        } catch (FileNotFoundException | UnsupportedEncodingException e) {
                throw new RuntimeException(e);
        }
        writer.println(getHeader(duration));
        writer.println();
        writer.println(getProbs());
        writer.println();
        writer.println(getModes());
        writer.println();
        writer.close();
	}

	private String getProbs() {
		StringBuilder builder = new StringBuilder();
		for (ProblemStats stat : problemstats) {
			builder.append("problem ").append(String.format("%02d", stat.id)).append(":    ");
			builder.append(String.format("%05d", stat.points / stat.seeds)).append(" points    ");
			builder.append(String.format("%03d", stat.seconds)).append(" seconds (").append(String.format("%03d", stat.seconds / stat.seeds)).append(" average)   ");
			builder.append(String.format("%02d", stat.seeds)).append(" seeds     Solving modes:   ");
			if (stat.modes.size() > 2) {
				builder.append(stat.modes.size()).append(" different");
			} else {
				builder.append(stat.modes);
			}
			builder.append("\n");
		}
		return builder.toString();
	}

	private String getModes() {
		StringBuilder builder = new StringBuilder();
		for (ModeStats stat : modestats) {
			builder.append(stat.mode.name()).append(":   ");
			builder.append(stat.wins).append(" wins and ");
			builder.append(stat.points / stat.used).append(" average points");
			builder.append("\n");
		}
		return builder.toString();
	}

	private String getHeader(long duration) {
		StringBuilder builder = new StringBuilder();
		builder.append(Main.TAG);
		builder.append("   ").append(Math.ceil(duration / 1000)).append(" seconds");
		builder.append("   ").append(seeds).append(" seeds in ").append(problemstats.length).append(" problems");
		builder.append("   ").append(total_points).append(" total points using ").append(modestats.length).append(" modes");
		return builder.toString();
	}

	private void processSolutions() {
		SortedSet<Integer> problems = new TreeSet<>();
		SortedSet<PathFinder.Mode> modes = new TreeSet<>();
		for (Solution solution : solutions) {
			problems.add(solution.id);
			modes.add(solution.mode);
			seeds++;
			total_points += solution.points;
		}
		
		problemstats = new ProblemStats[problems.size()];
		modestats = new ModeStats[modes.size()];
		for (int i = 0; i < problemstats.length; i++) {
			problemstats[i] = new ProblemStats();
			problemstats[i].modes = new TreeSet<>();
		}
		for (int i = 0; i < modestats.length; i++) {
			modestats[i] = new ModeStats();
		}
		
		for (Solution solution : solutions) {
			int prob = problems.headSet(solution.id).size();
			problemstats[prob].points += solution.points;
			problemstats[prob].seeds += 1;
			problemstats[prob].modes.add(solution.mode);
			problemstats[prob].id = solution.id;
			problemstats[prob].seconds += solution.seconds;
			
			int mod = modes.headSet(solution.mode).size();
			modestats[mod].points += solution.points;
			modestats[mod].wins += 1;
			modestats[mod].used += 1;
			modestats[mod].mode = solution.mode;
			
			//todo evaluate all the subsolutions which did not win, once they get along in the solution
		}
		
	}
	
	private class ProblemStats {
		int id;
		int points = 0;
		int seeds = 0;
		int seconds = 0;
		SortedSet<PathFinder.Mode> modes;
	}
	
	private class ModeStats {
		PathFinder.Mode mode;
		int wins = 0;
		int used = 0;
		int points = 0;
	}


}
