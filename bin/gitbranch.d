#!/usr/bin/env rdmd

import std.stdio;
import std.process;
import std.path;
import std.file;
import std.string;
import std.algorithm;
import std.getopt;
import std.datetime;
import std.conv;

class CGit
{
	string		mRoot;

	this(string root)
	{
		mRoot = root;
	}

	auto		opCall(string[] args...)
	{
		return std.process.execute(["git"] ~ args, null, Config.none, size_t.max, mRoot);
	}

	string		get(string[] args...)
	{
		auto status = this.opCall(args);
		if (status.status != 0)
		{
			writeln(status.output);
			return "";
		}
		return status.output;
	}
}

CGit		gGit;

class CBranch
{
	string		mName;
	SysTime		mDate;

	this(string name)
	{
		mName = name;
		string	date = gGit.get("log", "--max-count=1", "--format=%aD", mName).strip;
		mDate = parseRFC822DateTime(date);
	}
};

class CPerAuthor
{
	string		mAuther;
	CBranch[]	mBranches;
};

struct SArgs
{
	bool		parse(string[] args)
	{
		return true;
	}
};

string	durrToString(in Duration durr)
{
	string	o;
	int		years, weeks, days, hours;
	durr.split!("weeks", "days", "hours")(weeks, days, hours);

	days = days + weeks * 7;

	years = days / 365;
	days = days % 365;
	weeks = days / 7;
	days = days % 7;

	if (years > 0)
		o ~= to!string(years) ~ " years ";
	if (weeks > 0)
		o ~= to!string(weeks) ~ " weeks ";
	if (days > 0)
		o ~= to!string(days) ~ " days ";
	if (hours > 0)
		o ~= to!string(hours) ~ " hours ";
	return o;
}

int main(string[] av)
{
	SArgs		args;
	if (!args.parse(av))
		return 1;

	string		dir = std.file.getcwd();
	gGit = new CGit(dir);

	long		maxBrLen = 0;

	CPerAuthor[string]	authors;
	string[]			branchCmd = [ "branch" ];
	if (av.length > 1)
		branchCmd ~= av[1..av.length];
	else
		branchCmd ~= ["-a", "-r", "--merged"];

	writeln("Git command : git ", branchCmd.join(" "));
	foreach (string branch; gGit.get(branchCmd).splitLines())
	{
		branch = branch[2..branch.length];
		long		space = branch.indexOf(' ');
		if (space > 0)
			branch = branch[0..space];

		if (branch.find("HEAD") != [])
			continue;

		string	headH = gGit.get("rev-list", "--max-count=1" , branch).strip;
		if (!headH.length)
			continue;

		string	author = gGit.get("log", "--max-count=1", "--format=%an", headH).strip;

		if (author !in authors)
		{
			CPerAuthor		a = new CPerAuthor;
			a.mAuther = author;
			authors[author] = a;
		}
		CPerAuthor	*pera = &(authors[author]);

		pera.mBranches ~= [ new CBranch(branch) ];

		maxBrLen = max(maxBrLen, branch.length);
		//writeln(branch, " > ", author);
	}

	bool		sorter(in CPerAuthor a, in CPerAuthor b) @safe pure nothrow { return a.mBranches.length > b.mBranches.length; }
	auto		authorsArray = sort!(sorter)(authors.values);

	SysTime		now = Clock.currTime;

	foreach (ref CPerAuthor pera; authorsArray)
	{
		writeln(pera.mAuther);
		bool	branchSorter(in CBranch a, in CBranch b) { return a.mDate > b.mDate; }
		sort!branchSorter(pera.mBranches);
		foreach (branch; pera.mBranches)
		{
			write("  ");
			write(branch.mName);
			write(" ");
			foreach (i; 0..(maxBrLen - branch.mName.length))
				write(" ");
			writeln(durrToString(now - branch.mDate));
		}
	}
	return 0;
}
