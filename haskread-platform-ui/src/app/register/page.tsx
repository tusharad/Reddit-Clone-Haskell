'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import Header from '../components/Header';

const Register = () => {
  const [userName, setUserName] = useState('');
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [confirmPassword, setConfirmPassword] = useState('');
  const [error, setError] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const router = useRouter();

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setIsLoading(true); // Start loading

    try {
      const response = await fetch('http://localhost:8085/api/v1/user/auth/register', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          userNameForRegister: userName,
          emailForRegister: email,
          passwordForRegister: password,
          confirmPasswordForRegister: confirmPassword,
        }),
      });

      if (response.status === 400) {
        const res = await response.text();
        setError(res || 'Registration failed');
      } else if (response.ok) {
        const data = await response.json();
        router.push(`/user/verify/${data.userIDForRUR}`);
      }
    } catch (error) {
      setError('An error occurred during registration: ' + error);
    } finally {
      setIsLoading(false); // Stop loading
    }
  };

  return (
    <div className="flex flex-col min-h-screen bg-white">
      <Header setIsLoggedIn={() => {}} />
      <main className="container mx-auto mt-20 px-6 flex-grow">
        <h1 className="text-2xl font-bold mb-4 text-center">Register</h1>
        {error && <p className="text-red-500 text-sm mb-4">{error}</p>}
        <div className="card-bg px-6 py-6 shadow-lg rounded-lg mb-6 overflow-hidden">
          {isLoading && (
            <div className="text-center mb-4 text-gray-700">
              Registering...
            </div>
          )}
          <form onSubmit={handleSubmit}>
            <div className="mb-4">
              <label htmlFor="userName" className="block text-gray-700">Username</label>
              <input
                type="text"
                id="userName"
                value={userName}
                onChange={(e) => setUserName(e.target.value)}
                className="w-full px-3 py-2 border rounded"
                required
              />
            </div>
            <div className="mb-4">
              <label htmlFor="email" className="block text-gray-700">Email</label>
              <input
                type="email"
                id="email"
                value={email}
                onChange={(e) => setEmail(e.target.value)}
                className="w-full px-3 py-2 border rounded"
                required
              />
            </div>
            <div className="mb-4">
              <label htmlFor="password" className="block text-gray-700">Password</label>
              <input
                type="password"
                id="password"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                className="w-full px-3 py-2 border rounded"
                required
              />
            </div>
            <div className="mb-4">
              <label htmlFor="confirmPassword" className="block text-gray-700">Confirm Password</label>
              <input
                type="password"
                id="confirmPassword"
                value={confirmPassword}
                onChange={(e) => setConfirmPassword(e.target.value)}
                className="w-full px-3 py-2 border rounded"
                required
              />
            </div>
            <button
              type="submit"
              className="w-full bg-blue-500 text-white py-2 rounded hover:bg-blue-600"
              disabled={isLoading}
            >
              {isLoading ? 'Registering...' : 'Register'}
            </button>
          </form>
        </div>
      </main>
    </div>
  );
};

export default Register;