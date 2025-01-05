namespace Csharp2024;

using System;
using System.Collections.Generic;

public class Dec16
{
    private const int Day = 16;
    private static char[][] GetMaze(string text)
    {
        var lines = text.Split('\n', StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries);
        
        var maze = lines.Select(l=>l.ToCharArray()).ToArray();
        return maze;
    }

    private static char[][] GetMazeFromFile()
    {
        var filename = MetaUtils.todayFilename(Day);
        var text = File.ReadAllText(filename);
        return GetMaze(text);
    }

    private static ((int, int), (int, int)) GetStartEnd(char[][] maze)
    {
        var start = (-1, -1);
        var end = (-1, -1);
        var rows = maze.Length;
        var cols = maze[0].Length;
        for (int row = 0; row < rows; row++)
        {
            for (int col = 0; col < cols; col++)
            {
                if (maze[row][col] == 'S')
                    start = (row, col);
                if (maze[row][col] == 'E')
                    end = (row, col);
            }
        }
        return (start, end);
    }
    
    private class CurrentState(int row, int col, int direction)
    {
        public int Row { get; } = row;
        public int Col { get; } = col;
        public int Direction { get; set; } = direction;
        public override int GetHashCode()
        {
            return this.Row * 100000 + this.Col;
        }

        public override bool Equals(object? obj)
        {
            if (!(obj is CurrentState other)) return false;
            return this.Row == other.Row && this.Col == other.Col && this.Direction == other.Direction;
            
        }
        
    }
    
    private static readonly (int, int)[] Directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]; //up, right, down, left

    private static int Dijkstra(char[][] maze)
    {
        var ((sr,sc),(er,ec)) = GetStartEnd(maze);
        var q = new PriorityQueue<CurrentState?, int>();
        var seen = new HashSet<CurrentState?>();
        q.Enqueue(new CurrentState(sr, sc, 1), 0);
        while (q.TryDequeue(out var current, out var d))
        {
            if (current == null) continue;
            if ((current.Row, current.Col) == (er, ec))
                return d;
            if (!seen.Add(current))
                continue;
            var (dr, dc) = Directions[current.Direction];
            var rr = current.Row + dr;
            var cc = current.Col + dc;
            if (rr >= 0 && rr < maze.Length && cc >= 0 && cc < maze[0].Length && maze[rr][cc] != '#')
            {
                q.Enqueue(new CurrentState(rr, cc, current.Direction),d+1);
            }
            q.Enqueue(new CurrentState(current.Row, current.Col, (current.Direction+1) % 4),d+1000);
            q.Enqueue(new CurrentState(current.Row, current.Col, (current.Direction+3) % 4),d+1000);
        }
        return 0;
    }

    public static void Del1()
    {
        var maze = GetMazeFromFile();
        var best = Dijkstra(maze);
        Console.WriteLine($"C# Part1: {best}");
    }

    public static void Del1Ex()
    {
        var maze = GetMaze(@"#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################");
        var best = Dijkstra(maze);
        Console.WriteLine($"C# Part1ex: {best}");
    }


}