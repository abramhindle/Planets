Summary: A simple and fun planetary simulator
Name: planets
Version: __VERSION__
Release: 1
URL: http://planets.homedns.org
Source0: %{name}-%{version}.tgz
License: GPL
Group: Amusements/Games
BuildRoot: %{_tmppath}/%{name}-root

%description 
Planets is a simple interactive program for playing with
simulations of planetary systems.  The user interface is
aimed at being simple enough that a fairly young kid can get
some joy out of it.  But it's probably as much if not more
fun for adults.

%prep
%setup -q

%build
make all

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/bin
mkdir -p $RPM_BUILD_ROOT/usr/share/applications
mkdir -p $RPM_BUILD_ROOT/usr/share/applnk/Games
mkdir -p $RPM_BUILD_ROOT/usr/share/pixmaps
mkdir -p $RPM_BUILD_ROOT/usr/share/man/man1
make PREFIX=$RPM_BUILD_ROOT/usr install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc KEYBINDINGS.txt LICENSE CHANGES BUGS CREDITS getting_started.html
/usr/bin/*
/usr/share/applications/*
/usr/share/applnk/Games/*
/usr/share/pixmaps/*
/usr/share/man/man1/*

%changelog
* Mon Apr 21 2003 Yaron M. Minsky <yminsky@cs.cornell.edu>
- modified spec file to use "make install"
* Mon Feb 25 2002 Yaron M. Minsky <yminsky@cs.cornell.edu>
- Initial build.



