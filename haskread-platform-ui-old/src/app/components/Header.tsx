'use client'

import React, { useEffect, useState } from 'react';
import Link from 'next/link';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faMagnifyingGlass } from '@fortawesome/free-solid-svg-icons';
import CreateThreadModal from './CreateThreadModal';

interface HeaderProps {
  setIsLoggedIn: (loggedIn: number | null) => void;
  setNewThreadAdded: (newThreadAdded: boolean) => void;
}

const Header: React.FC<HeaderProps> = ({ setIsLoggedIn, setNewThreadAdded }) => {
  const [userName, setUserName] = useState<string | null>(null);
  const [isModalOpen, setIsModalOpen] = useState(false);

  useEffect(() => {

    const fetchData = async () => {
        const token = localStorage.getItem('jwt_token');
        if (token) {
        const user = JSON.parse(atob(token.split('.')[1]));
        setUserName(user.dat.userNameForUserInfo);
        setIsLoggedIn(user.dat.userIDForUserInfo);
      } else {
      setUserName(null);
      setIsLoggedIn(null);
      }
    }
    fetchData();
  }, [setIsLoggedIn]);

  const logOut = () => {
    setIsLoggedIn(null);
    setUserName(null);
    localStorage.removeItem('jwt_token');
  };

  return (
    <header className="navbar-bg shadow-lg w-full z-50">
      <div className="container mx-auto px-6 py-4 flex justify-between items-center">
        <Link href="/" className="text-3xl font-bold text-white">
          HaskRead
        </Link>
        <div className="flex items-center">
          <div className="relative">
            <input
              type="text"
              className="pl-2 pr-4 py-2 border rounded-full focus:outline-none focus:ring-2 focus:ring-blue-600"
              placeholder="Search thread, community or user"
            />
            <FontAwesomeIcon
              icon={faMagnifyingGlass}
              style={{ verticalAlign: '-0.8em' }}
              className="bg-white rounded-full px-3 py-3 ml-2"
            />
          </div>
          <div className="ml-4 flex items-center space-x-2">
            {userName ? (
              <>
                <button
                  onClick={() => setIsModalOpen(true)}
                  className="px-4 py-2 bg-green-600 text-white rounded-full font-semibold hover:bg-green-500 transition"
                >
                  Create Thread
                </button>
                <Link
                  href="/my-profile"
                  className="px-4 py-2 bg-blue-600 text-white rounded-full font-semibold hover:bg-blue-500 transition"
                >
                  {userName}
                </Link>
                <button
                  onClick={logOut}
                  className="px-4 py-2 bg-blue-600 text-white rounded-full font-semibold hover:bg-blue-500 transition"
                >
                  Logout
                </button>
              </>
            ) : (
              <>
                <Link
                  href="/register"
                  className="px-4 py-2 bg-blue-600 text-white rounded-full font-semibold hover:bg-blue-500 transition"
                >
                  Sign up
                </Link>
                <Link
                  href="/login"
                  className="px-4 py-2 bg-gray-200 text-gray-800 rounded-full hover:bg-gray-300 transition"
                >
                  Log in
                </Link>
              </>
            )}
          </div>
        </div>
      </div>
      {isModalOpen && (
        <CreateThreadModal
          closeModal={() => setIsModalOpen(false)}
          setNewThreadAdded={setNewThreadAdded}
        />
      )}
    </header>
  );
};

export default Header;
